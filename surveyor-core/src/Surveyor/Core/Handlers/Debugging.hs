{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Surveyor.Core.Handlers.Debugging ( handleDebuggingEvent ) where

import           Control.Lens ( (^.), (^?), _Just )
import qualified Control.Lens as L
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import qualified Data.IORef as IOR
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import           GHC.Stack ( HasCallStack )
import qualified Lang.Crucible.Simulator.ExecutionTree as LCSET
import qualified Prettyprinter as PP

import qualified Surveyor.Core.Architecture as SCA
import qualified Surveyor.Core.Events as SCE
import qualified Surveyor.Core.HandlerMonad as SCH
import qualified Surveyor.Core.Logging as SCL
import qualified Surveyor.Core.State as SCS
import qualified Surveyor.Core.SymbolicExecution as SymEx
import qualified Surveyor.Core.SymbolicExecution.ExecutionFeature as SCEF

noSessionFor :: (MonadIO m) => SCS.S e u arch s -> SymEx.SessionID s -> m ()
noSessionFor s0 sessionID = do
  let msg = SCL.msgWithContext { SCL.logText = [ PP.pretty "No session for SessionID " <> PP.pretty sessionID
                                               ]
                               , SCL.logSource = SCL.EventHandler (T.pack "Debug")
                               , SCL.logLevel = SCL.Warn
                               }
  liftIO $ SCS.logMessage s0 msg

sessionStateUnexpected :: (MonadIO m) => SCS.S e u arch s -> SymEx.SessionID s -> String -> m ()
sessionStateUnexpected s0 sessionID expectedState = do
  let msg = SCL.msgWithContext { SCL.logText = [ PP.pretty "Symbolic execution session is not " <> PP.pretty expectedState <> PP.pretty ": " <> PP.pretty sessionID
                                               ]
                               , SCL.logSource = SCL.EventHandler (T.pack "Debug")
                               , SCL.logLevel = SCL.Warn
                               }
  liftIO $ SCS.logMessage s0 msg

handleDebuggingEvent :: ( SCA.Architecture arch s
                        , SCA.CrucibleExtension arch
                        , MonadIO m
                        , HasCallStack
                        )
                     => SCS.S e u arch s
                     -> SCE.DebuggingEvent s (SCS.S e u)
                     -> SCH.HandlerT (SCS.State e u s) m (SCS.State e u s)
handleDebuggingEvent s0 evt =
  case evt of
    SCE.StepExecution sessionID -> do
      symExSt <- SCH.expectValue (s0 ^? SCS.lArchState . _Just . SCS.symExStateL)
      Some symEx <- SCH.expectValueWith (SymEx.lookupSessionState symExSt sessionID) $ do
        noSessionFor s0 sessionID

      SCH.withFailAction (sessionStateUnexpected s0 sessionID "Suspended") $ do
        SymEx.Suspended _symNonce suspSt <- return symEx
        let msg = SCL.msgWith { SCL.logText = [ PP.pretty "Stepping session " <> PP.pretty sessionID
                                              ]
                              }
        liftIO $ SCS.logMessage s0 msg
        -- To single step, we set the debug execution feature into its
        -- monitoring mode, which will cause it to stop at every state and
        -- send it to surveyor.
        --
        -- We then resume execution to restart the process
        let execFeatureStateRef = SymEx.suspendedDebugFeatureConfig suspSt
        liftIO $ SCEF.modifyDebuggerState execFeatureStateRef (setDebugState SCEF.Monitoring)
        liftIO $ SymEx.suspendedResumeUnmodified suspSt

        -- Switch the symbolic execution UI to the executing state
        switchToExecutingState s0 symEx

        return $! SCS.State s0

    SCE.StepOutExecution sessionID -> do
      symExSt <- SCH.expectValue (s0 ^? SCS.lArchState . _Just . SCS.symExStateL)
      Some symEx <- SCH.expectValueWith (SymEx.lookupSessionState symExSt sessionID) $ do
        noSessionFor s0 sessionID
      SCH.withFailAction (sessionStateUnexpected s0 sessionID "Suspended") $ do
        SymEx.Suspended _symNonce suspSt <- return symEx

        let execFeatureStateRef = SymEx.suspendedDebugFeatureConfig suspSt
        stackDepthRef <- liftIO $ IOR.newIORef 0
        liftIO $ SCEF.modifyDebuggerState execFeatureStateRef (setDebugState (SCEF.InactiveUntil (stepOutP stackDepthRef)))
        liftIO $ SymEx.suspendedResumeUnmodified suspSt
        switchToExecutingState s0 symEx

        return $! SCS.State s0

    SCE.ContinueExecution sessionID -> do
      symExSt <- SCH.expectValue (s0 ^? SCS.lArchState . _Just . SCS.symExStateL)
      Some symEx <- SCH.expectValueWith (SymEx.lookupSessionState symExSt sessionID) $ do
        noSessionFor s0 sessionID
      SCH.withFailAction (sessionStateUnexpected s0 sessionID "Suspended") $ do
        SymEx.Suspended _symNonce suspSt <- return symEx
        let msg = SCL.msgWith { SCL.logText = [ PP.pretty "Stepping session " <> PP.pretty sessionID
                                              ]
                              }
        liftIO $ SCS.logMessage s0 msg

        let execFeatureStateRef = SymEx.suspendedDebugFeatureConfig suspSt
        liftIO $ SCEF.modifyDebuggerState execFeatureStateRef (setDebugState SCEF.Inactive)
        liftIO $ SymEx.suspendedResumeUnmodified suspSt

        switchToExecutingState s0 symEx

        return $! SCS.State s0


    SCE.InterruptExecution sessionID -> do
      symExSt <- SCH.expectValue (s0 ^? SCS.lArchState . _Just . SCS.symExStateL)
      Some symEx <- SCH.expectValueWith (SymEx.lookupSessionState symExSt sessionID) $ do
        noSessionFor s0 sessionID
      SCH.withFailAction (sessionStateUnexpected s0 sessionID "Executing") $ do
        SymEx.Executing progress <- return symEx
        -- Interrupt execution by switching the monitor on.  The monitor will
        -- send an event at its next opportunity.  That event will trigger a
        -- state change to the suspended execution viewer.
        let execFeatureStateRef = SymEx.executionInterrupt progress
        liftIO $ SCEF.modifyDebuggerState execFeatureStateRef (setDebugState SCEF.Monitoring)

        return $! SCS.State s0


    SCE.EnableRecording sessionID -> do
      symExSt <- SCH.expectValue (s0 ^? SCS.lArchState . _Just . SCS.symExStateL)
      Some symEx <- SCH.expectValueWith (SymEx.lookupSessionState symExSt sessionID) $ do
        noSessionFor s0 sessionID
      SCH.withFailAction (sessionStateUnexpected s0 sessionID "Suspended") $ do
        SymEx.Suspended _ suspSt <- return symEx
        traceRef <- liftIO $ IOR.newIORef mempty
        _actualRef <- liftIO $ SCEF.modifyDebuggerState (SymEx.suspendedDebugFeatureConfig suspSt) (enableRecording traceRef)
        return $! SCS.State s0


    SCE.DisableRecording sessionID -> do
      symExSt <- SCH.expectValue (s0 ^? SCS.lArchState . _Just . SCS.symExStateL)
      Some symEx <- SCH.expectValueWith (SymEx.lookupSessionState symExSt sessionID) $ do
        noSessionFor s0 sessionID
      SCH.withFailAction (sessionStateUnexpected s0 sessionID "Suspended") $ do
        SymEx.Suspended symNonce suspSt <- return symEx
        let stateRef = SymEx.suspendedDebugFeatureConfig suspSt
        liftIO $ SCEF.modifyDebuggerState stateRef disableRecording
        -- If there are any states that have been recorded, save them into the
        -- state for review
        liftIO $ SCEF.withRecordedStates stateRef $ \mstates -> do
          case mstates of
            Nothing -> return ()
            Just recordedStates -> do
              let suspSt1 = suspSt { SymEx.suspendedHistory = Just (SymEx.RecordedStateLog 0 recordedStates) }
              liftIO $ SCS.sEmitEvent s0 (SCE.UpdateSymbolicExecutionState (s0 ^. SCS.lNonce) (SymEx.Suspended symNonce suspSt1))
        return $! SCS.State s0


enableRecording :: IOR.IORef (Seq.Seq (Some (LCSET.ExecState p sym ext)))
                -> Some (SCEF.DebuggerFeatureState p sym ext)
                -> (Some (SCEF.DebuggerFeatureState p sym ext), IOR.IORef (Seq.Seq (Some (LCSET.ExecState p sym ext))))
enableRecording newRef (Some r) =
  case r of
    SCEF.Recording currentRef _nestedState -> (Some r, currentRef)
    SCEF.Monitoring -> (Some (SCEF.Recording newRef r), newRef)
    SCEF.Inactive -> (Some (SCEF.Recording newRef r), newRef)
    SCEF.InactiveUntil {} -> (Some (SCEF.Recording newRef r), newRef)

disableRecording :: Some (SCEF.DebuggerFeatureState p sym ext)
                 -> (Some (SCEF.DebuggerFeatureState p sym ext), ())
disableRecording (Some r) =
  case r of
    SCEF.Recording _currentRef nestedState -> (Some nestedState, ())
    SCEF.Monitoring -> (Some r, ())
    SCEF.Inactive -> (Some r, ())
    SCEF.InactiveUntil {} -> (Some r, ())

-- | Set the debugger state to the given value, while keeping the current state
-- recording configuration
setDebugState :: SCEF.DebuggerFeatureState p sym ext SCEF.Normal
              -> Some (SCEF.DebuggerFeatureState p sym ext)
              -> (Some (SCEF.DebuggerFeatureState p sym ext), ())
setDebugState newState (Some s) =
  case s of
    SCEF.Recording ref _ -> (Some (SCEF.Recording ref newState), ())
    SCEF.Monitoring -> (Some newState, ())
    SCEF.Inactive -> (Some newState, ())
    SCEF.InactiveUntil {} -> (Some newState, ())

-- | Return True once execution returns from the current function
--
-- Operationally, increment the stack depth on every call, decrement it on every
-- return.  If we see a return when the depth is 0, we can return.
stepOutP :: IOR.IORef Int -> LCSET.ExecState p sym ext rtp -> IO Bool
stepOutP callDepthRef st =
  case st of
    LCSET.CallState {} -> do
      IOR.atomicModifyIORef' callDepthRef (\d -> (d + 1, ()))
      return False
    LCSET.TailCallState {} -> do
      IOR.atomicModifyIORef' callDepthRef (\d -> (d + 1, ()))
      return False
    LCSET.ReturnState {} -> do
      depth <- IOR.readIORef callDepthRef
      case depth of
        0 -> return True
        _ -> do
          IOR.atomicModifyIORef' callDepthRef (\d -> (d - 1, ()))
          return False
    _ -> return False

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
                                           , SymEx.executionResume = SymEx.suspendedResumeUnmodified suspSt
                                           }
  let exState = SymEx.Executing exProgress
  liftIO $ SCS.sEmitEvent s0 (SCE.UpdateSymbolicExecutionState (s0 ^. SCS.lNonce) exState)
