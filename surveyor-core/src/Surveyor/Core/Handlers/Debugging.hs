{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Surveyor.Core.Handlers.Debugging ( handleDebuggingEvent ) where

import           Control.Lens ( (^?), _Just )
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import qualified Data.IORef as IOR
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Text as T
import           GHC.Stack ( HasCallStack )

import qualified Surveyor.Core.Architecture as SCA
import qualified Surveyor.Core.Events as SCE
import qualified Surveyor.Core.Logging as SCL
import qualified Surveyor.Core.State as SCS
import qualified Surveyor.Core.SymbolicExecution as SymEx
import qualified Surveyor.Core.SymbolicExecution.ExecutionFeature as SCEF

handleDebuggingEvent :: ( SCA.Architecture arch s
                        , SCA.CrucibleExtension arch
                        , MonadIO m
                        , HasCallStack
                        )
                     => SCS.S e u arch s
                     -> SCE.DebuggingEvent s (SCS.S e u)
                     -> m (SCS.State e u s)
handleDebuggingEvent s0 evt =
  case evt of
    SCE.StepExecution sessionID
      | Just symExSt <- s0 ^? SCS.lArchState . _Just . SCS.symExStateL
      , Just (Some (SymEx.Suspended _symNonce suspSt)) <- SymEx.lookupSessionState symExSt sessionID -> do
          let msg = SCL.msgWith { SCL.logText = [ T.pack ("Stepping session " ++ show sessionID)
                                                ]
                                }
          liftIO $ SCS.logMessage s0 msg
          -- To single step, we set the debug execution feature into its
          -- monitoring mode, which will cause it to stop at every state and
          -- send it to surveyor.
          --
          -- We then resume execution to restart the process
          let execFeatureStateRef = SymEx.suspendedDebugFeatureConfig suspSt
          liftIO $ IOR.atomicWriteIORef execFeatureStateRef SCEF.Monitoring

          liftIO $ SymEx.suspendedResumeUnmodified suspSt

          -- This doesn't immediately update the state - that only happens when
          -- Crucible stops and sends us a new state.  We could potentially add
          -- a UI indicator that execution is happening.
          return $! SCS.State s0
      | otherwise -> return $! SCS.State s0

    SCE.ContinueExecution sessionID
      | Just symExSt <- s0 ^? SCS.lArchState . _Just . SCS.symExStateL
      , Just (Some (SymEx.Suspended _symNonce suspSt)) <- SymEx.lookupSessionState symExSt sessionID -> do
          let msg = SCL.msgWith { SCL.logText = [ T.pack ("Stepping session " ++ show sessionID)
                                                ]
                                }
          liftIO $ SCS.logMessage s0 msg

          let execFeatureStateRef = SymEx.suspendedDebugFeatureConfig suspSt
          liftIO $ IOR.atomicWriteIORef execFeatureStateRef SCEF.Inactive
          liftIO $ SymEx.suspendedResumeUnmodified suspSt

          return $! SCS.State s0
      | otherwise -> return $! SCS.State s0

    SCE.InterruptExecution sessionID
      | Just symExSt <- s0 ^? SCS.lArchState . _Just . SCS.symExStateL
      , Just (Some (SymEx.Suspended _symNonce suspSt)) <- SymEx.lookupSessionState symExSt sessionID -> do
          let execFeatureStateRef = SymEx.suspendedDebugFeatureConfig suspSt
          liftIO $ IOR.atomicWriteIORef execFeatureStateRef SCEF.Monitoring

          return $! SCS.State s0
      | otherwise -> return $! SCS.State s0
