{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
module Surveyor.Core.SymbolicExecution.ExecutionFeature (
  DebuggerConfig(..),
  debuggerConfigStateVar,
  newDebuggerConfig,
  DebuggerFeatureState(..),
  debuggerFeature,
  CrucibleExecState(..),
  ReturnExecState(..)
  ) where

import qualified Control.Concurrent.Chan as CCC
import qualified Data.IORef as DI
import qualified Data.Parameterized.Classes as PC
import qualified Data.Parameterized.Nonce as PN
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

data DebuggerFeatureState where
  -- | Set the debugger feature to send all events to the debugger
  Monitoring :: DebuggerFeatureState
  -- | Set the debugger feature to be inactive and just allow the symbolic execution to proceed normally
  Inactive :: DebuggerFeatureState
  -- | Allow symbolic execution to proceed normally until the predicate returns
  -- True, at which point the feature switches into 'Monitoring' mode
  InactiveUntil :: (forall p sym ext rtp . LCSET.ExecState p sym ext rtp -> IO Bool) -> DebuggerFeatureState

data DebuggerConfig s p sym arch ext where
  DebuggerConfig :: ( ext ~ SCA.CrucibleExt arch
                    , sym ~ WEB.ExprBuilder s st fs
                    , CB.IsSymInterface sym
                    )
                 => PN.Nonce s arch
                 -> SCSSe.SessionID s
                 -> CCC.Chan (Maybe (CrucibleExecState s p sym ext))
                 -> CCC.Chan (ReturnExecState s p sym ext)
                 -> DI.IORef DebuggerFeatureState
                 -> DebuggerConfig s p sym arch ext

debuggerConfigStateVar :: DebuggerConfig s p sym arch ext -> DI.IORef DebuggerFeatureState
debuggerConfigStateVar (DebuggerConfig _ _ _ _ v) = v

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
  r <- DI.newIORef Inactive
  return (DebuggerConfig archNonce sessionID c1 c2 r)

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
debugger conf@(DebuggerConfig _ _ toDebugger fromDebugger stateRef) ng estate = do
  s <- DI.readIORef stateRef
  case s of
    Inactive -> return LCS.ExecutionFeatureNoChange
    InactiveUntil p -> do
      shouldMonitor <- p estate
      if | shouldMonitor -> do
             -- Once the predicate becomes true, switch to 'Monitoring' mode and
             -- handle it with a recursive call
             DI.writeIORef stateRef Monitoring
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
