{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Surveyor.Core.Handlers.Debugging ( handleDebuggingEvent ) where

import           Control.Lens ( (^.), (^?), _Just )
import qualified Control.Lens as L
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import qualified Data.IORef as IOR
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Text as T
import           GHC.Stack ( HasCallStack )
import qualified Lang.Crucible.Simulator.ExecutionTree as LCSET

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
      , Just (Some symEx@(SymEx.Suspended _symNonce suspSt)) <- SymEx.lookupSessionState symExSt sessionID -> do
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

          switchToExecutingState s0 symEx

          return $! SCS.State s0
      | otherwise -> return $! SCS.State s0

    SCE.ContinueExecution sessionID
      | Just symExSt <- s0 ^? SCS.lArchState . _Just . SCS.symExStateL
      , Just (Some symEx@(SymEx.Suspended _symNonce suspSt)) <- SymEx.lookupSessionState symExSt sessionID -> do
          let msg = SCL.msgWith { SCL.logText = [ T.pack ("Stepping session " ++ show sessionID)
                                                ]
                                }
          liftIO $ SCS.logMessage s0 msg

          let execFeatureStateRef = SymEx.suspendedDebugFeatureConfig suspSt
          liftIO $ IOR.atomicWriteIORef execFeatureStateRef SCEF.Inactive
          liftIO $ SymEx.suspendedResumeUnmodified suspSt

          switchToExecutingState s0 symEx

          return $! SCS.State s0
      | otherwise -> return $! SCS.State s0

    SCE.InterruptExecution sessionID
      | Just symExSt <- s0 ^? SCS.lArchState . _Just . SCS.symExStateL
      , Just (Some (SymEx.Executing progress)) <- SymEx.lookupSessionState symExSt sessionID -> do
          -- Interrupt execution by switching the monitor on.  The monitor will
          -- send an event at its next opportunity.  That event will trigger a
          -- state change to the suspended execution viewer.
          let execFeatureStateRef = SymEx.executionInterrupt progress
          liftIO $ IOR.atomicWriteIORef execFeatureStateRef SCEF.Monitoring

          return $! SCS.State s0
      | otherwise -> return $! SCS.State s0

-- | Construct an 'SymEx.Executing' state (from the suspended state) and switch
-- to it by sending an event
--
-- NOTE: This currently sets the metrics to zero (the previous metrics could be
-- stashed in the suspended state, potentially)
switchToExecutingState :: (MonadIO m)
                       => SCS.S e u arch s
                       -> SymEx.SymbolicExecutionState arch s SymEx.Suspend
                       -> m ()
switchToExecutingState s0 symEx@(SymEx.Suspended _nonce suspSt) = do
  let symConf = SymEx.symbolicExecutionConfig symEx
  let hdl = suspSt ^. L.to SymEx.suspendedSimState . LCSET.stateContext . L.to LCSET.printHandle
  let exProgress = SymEx.ExecutionProgress { SymEx.executionMetrics = SymEx.emptyMetrics
                                           , SymEx.executionOutputHandle = hdl
                                           , SymEx.executionConfig = symConf
                                           , SymEx.executionInterrupt = SymEx.suspendedDebugFeatureConfig suspSt
                                           }
  let exState = SymEx.Executing exProgress
  liftIO $ SCS.sEmitEvent s0 (SCE.UpdateSymbolicExecutionState (s0 ^. SCS.lNonce) exState)
