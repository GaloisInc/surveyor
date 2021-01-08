{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
module Surveyor.Core.SymbolicExecution.ExecutionFeature (
  DebuggerConfig(..),
  debuggerConfigStateVar,
  newDebuggerConfig,
  DebuggerFeatureState(..),
  Normal,
  Record,
  DebuggerStateRef,
  setDebuggerState,
  modifyDebuggerState,
  withRecordedStates,
  debuggerFeature,
  CrucibleExecState(..),
  ReturnExecState(..)
  ) where

import qualified Control.Concurrent.Chan as CCC
import qualified Control.Monad.Catch as CMC
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import qualified Data.IORef as DI
import qualified Data.Parameterized.Classes as PC
import qualified Data.Parameterized.Nonce as PN
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Sequence as Seq
import           GHC.Stack ( HasCallStack )
import qualified Lang.Crucible.Backend as CB
import qualified Lang.Crucible.Simulator.EvalStmt as LCS
import qualified Lang.Crucible.Simulator.ExecutionTree as LCSET
import qualified What4.Expr as WEB

import qualified Surveyor.Core.Architecture as SCA
import qualified Surveyor.Core.Panic as SCP
import qualified Surveyor.Core.SymbolicExecution.Session as SCSSe

-- | This is a container for the symbolic execution states being transferred
-- between the execution feature (crux) thread and the debugger.
--
-- We use a 'PN.Nonce' to witness the return type of the state, since there is
-- no representative to recover it normally (easily).  This becomes relevant
-- because we have one channel to send states from crux to the debugger, but
-- another to send (potentially modified) states back.  The 'PN.Nonce' is the
-- only way for us to prove that the two @rtp@s are the same.
data CrucibleExecState s p sym ext where
  CrucibleExecState :: PN.Nonce s rtp -> LCSET.ExecState p sym ext rtp -> CrucibleExecState s p sym ext

data ReturnExecState s p sym ext where
  UnmodifiedExecState :: ReturnExecState s p sym ext
  ModifiedExecState :: PN.Nonce s rtp -> LCSET.ExecState p sym ext rtp -> ReturnExecState s p sym ext

data DebuggerFeatureStateKind = Record | Normal
type Record = 'Record
type Normal = 'Normal

data DebuggerFeatureState p sym ext (k :: DebuggerFeatureStateKind) where
  -- | Set the debugger feature to send all events to the debugger
  Monitoring :: DebuggerFeatureState p sym ext Normal
  -- | Set the debugger feature to be inactive and just allow the symbolic execution to proceed normally
  Inactive :: DebuggerFeatureState p sym ext Normal
  -- | Allow symbolic execution to proceed normally until the predicate returns
  -- True, at which point the feature switches into 'Monitoring' mode
  InactiveUntil :: (forall rtp . LCSET.ExecState p sym ext rtp -> IO Bool) -> DebuggerFeatureState p sym ext Normal
  -- | Record observed states in the given sequence (delegating behavior to the included 'DebuggerFeatureState')
  Recording :: DI.IORef (Seq.Seq (Some (LCSET.ExecState p sym ext))) -> DebuggerFeatureState p sym ext Normal -> DebuggerFeatureState p sym ext Record

data DebuggerConfig s p sym arch ext where
  DebuggerConfig :: ( ext ~ SCA.CrucibleExt arch
                    , sym ~ WEB.ExprBuilder s st fs
                    , CB.IsSymInterface sym
                    )
                 => PN.Nonce s arch
                 -> SCSSe.SessionID s
                 -> CCC.Chan (Maybe (CrucibleExecState s p sym ext))
                 -> CCC.Chan (ReturnExecState s p sym ext)
                 -> DebuggerStateRef p sym ext
                 -> DebuggerConfig s p sym arch ext

data DebuggerStateRef p sym ext where
  DebuggerStateRef :: DI.IORef (Some (DebuggerFeatureState p sym ext)) -> DebuggerStateRef p sym ext

setDebuggerState :: DebuggerStateRef p sym ext -> DebuggerFeatureState p sym ext k -> IO ()
setDebuggerState (DebuggerStateRef r) s = DI.atomicWriteIORef r (Some s)

modifyDebuggerState :: DebuggerStateRef p sym ext
                    -> (Some (DebuggerFeatureState p sym ext) -> (Some (DebuggerFeatureState p sym ext), a))
                    -> IO a
modifyDebuggerState (DebuggerStateRef r) f =
  DI.atomicModifyIORef' r f

-- | Call the continuation with the recorded symbolic execution states (if any)
withRecordedStates :: (MonadIO m, CMC.MonadThrow m)
                   => DebuggerStateRef p sym ext
                   -> (Maybe (Seq.Seq (Some (LCSET.ExecState p sym ext))) -> m a)
                   -> m a
withRecordedStates (DebuggerStateRef r) k = do
  Some featState <- liftIO $ DI.readIORef r
  case featState of
    Recording ref _wrappedState -> do
      states <- liftIO $ DI.readIORef ref
      k (Just states)
    _ -> k Nothing

debuggerConfigStateVar :: DebuggerConfig s p sym arch ext -> DebuggerStateRef p sym ext
debuggerConfigStateVar (DebuggerConfig _ _ _ _ v) = v

-- | Create a fresh configuration for the given symbolic execution session
newDebuggerConfig :: ( ext ~ SCA.CrucibleExt arch
                     , sym ~ WEB.ExprBuilder s st fs
                     , CB.IsSymInterface sym
                     )
                  => PN.Nonce s arch
                  -> SCSSe.SessionID s
                  -> IO (DebuggerConfig s p sym arch ext)
newDebuggerConfig archNonce sessionID = do
  c1 <- CCC.newChan
  c2 <- CCC.newChan
  r <- DI.newIORef (Some Inactive)
  return (DebuggerConfig archNonce sessionID c1 c2 (DebuggerStateRef r))

-- | An execution feature enabling some debugging features in surveyor
--
-- This allows for stopping and resuming symbolic execution, as well as state
-- collection for replay debugging
debuggerFeature :: (sym ~ WEB.ExprBuilder s st fs)
                => DebuggerConfig s p sym arch ext
                -> PN.NonceGenerator IO s
                -> LCS.ExecutionFeature p sym ext rtp
debuggerFeature conf ng = LCS.ExecutionFeature (debugger conf ng)

debugger :: (sym ~ WEB.ExprBuilder s st fs, HasCallStack)
         => DebuggerConfig s p sym arch ext
         -> PN.NonceGenerator IO s
         -> LCSET.ExecState p sym ext rtp
         -> IO (LCS.ExecutionFeatureResult p sym ext rtp)
debugger conf@(DebuggerConfig _ _ _ _ (DebuggerStateRef stateRef)) ng estate = do
  Some s <- DI.readIORef stateRef
  case s of
    Recording states s' -> do
      -- If we are in record mode, prepend this state to the queue (the current state is always index 0)
      DI.modifyIORef' states (Some estate Seq.<|)
      doDebugAction conf ng estate s'
    Inactive -> doDebugAction conf ng estate s
    InactiveUntil _ -> doDebugAction conf ng estate s
    Monitoring -> doDebugAction conf ng estate s

doDebugAction :: (sym ~ WEB.ExprBuilder s st fs, HasCallStack)
              => DebuggerConfig s p sym arch ext
              -> PN.NonceGenerator IO s
              -> LCSET.ExecState p sym ext rtp
              -> DebuggerFeatureState p sym ext Normal
              -> IO (LCS.ExecutionFeatureResult p sym ext rtp)
doDebugAction conf@(DebuggerConfig _ _ toDebugger fromDebugger (DebuggerStateRef stateRef)) ng estate s =
  case s of
    Inactive -> return LCS.ExecutionFeatureNoChange
    InactiveUntil p -> do
      shouldMonitor <- p estate
      if | shouldMonitor -> do
             -- Once the predicate becomes true, switch to 'Monitoring' mode and
             -- handle it with a recursive call
             DI.writeIORef stateRef (Some Monitoring)
             debugger conf ng estate
         | otherwise -> return LCS.ExecutionFeatureNoChange
    Monitoring -> do
      rtp <- PN.freshNonce ng
      CCC.writeChan toDebugger (Just (CrucibleExecState rtp estate))
      res <- CCC.readChan fromDebugger
      case res of
        UnmodifiedExecState -> return LCS.ExecutionFeatureNoChange
        ModifiedExecState rtp' newState
          | Just PC.Refl <- PC.testEquality rtp' rtp -> do
              return (LCS.ExecutionFeatureModifiedState newState)
          | otherwise -> SCP.panic "DebuggerExecutionFeature" ["Execution feature channels out of sync"]
