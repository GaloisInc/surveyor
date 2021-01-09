{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Surveyor.Core.Commands (
  exitC,
  describeCommandC,
  describeKeysC,
  loadFileC,
  loadLLVMC,
  loadJARC,
  loadELFC,
  selectNextInstructionC,
  selectPreviousInstructionC,
  selectNextOperandC,
  selectPreviousOperandC,
  resetInstructionSelectionC,
  contextBackC,
  stepExecutionC,
  stepOutExecutionC,
  continueExecutionC,
  interruptExecutionC,
  disableRecordingC,
  enableRecordingC,
  stepTraceBackwardC,
  stepTraceForwardC,
  contextForwardC,
  initializeSymbolicExecutionC,
  beginSymbolicExecutionSetupC,
  startSymbolicExecutionC,
  setLogFileC,
  saveCurrentValueSVGC,
  disableFileLoggingC,
  nameValueC,
  nameCurrentValueC,
  allCommands
  ) where

import           Control.Lens ( (^.), (^?) )
import qualified Data.Functor.Const as C
import qualified Data.Parameterized.List as PL
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Text as T
import qualified Lang.Crucible.CFG.Core as CCC
import qualified Prettyprinter as PP

import qualified Surveyor.Core.Architecture as CA
import qualified Surveyor.Core.Arguments as AR
import qualified Surveyor.Core.Chan as C
import qualified Surveyor.Core.Command as C
import qualified Surveyor.Core.Context as CCX
import qualified Surveyor.Core.Events as SCE
import qualified Surveyor.Core.Logging as SCL
import qualified Surveyor.Core.State as CS
import qualified Surveyor.Core.SymbolicExecution as SymEx

-- | This is a trivial wrapper to force the type of its argument to 'T.Text';
-- this is really useful because @-XOverloadedStrings@ is enabled, and string
-- literals are otherwise too polymorphic to use with the prettyprinter
text :: T.Text -> PP.Doc ann
text = PP.pretty

type Command s st tps = C.Command (AR.SurveyorCommand s st) tps
type Callback s st tps = C.Chan (SCE.Events s st)
                      -> AR.SomeState st s
                      -> PL.List (C.ArgumentType (AR.SurveyorCommand s st)) tps
                      -> IO ()

allCommands :: (AR.HasNonce st, st ~ CS.S e u) => [C.SomeCommand (AR.SurveyorCommand s st)]
allCommands =
  [ C.SomeCommand exitC
  , C.SomeCommand describeCommandC
  , C.SomeCommand describeKeysC
  , C.SomeCommand loadFileC
  , C.SomeCommand loadELFC
  , C.SomeCommand loadLLVMC
  , C.SomeCommand loadJARC
  , C.SomeCommand selectNextInstructionC
  , C.SomeCommand selectPreviousInstructionC
  , C.SomeCommand selectNextOperandC
  , C.SomeCommand selectPreviousOperandC
  , C.SomeCommand resetInstructionSelectionC
  , C.SomeCommand contextBackC
  , C.SomeCommand contextForwardC
  , C.SomeCommand stepOutExecutionC
  , C.SomeCommand stepExecutionC
  , C.SomeCommand continueExecutionC
  , C.SomeCommand interruptExecutionC
  , C.SomeCommand enableRecordingC
  , C.SomeCommand disableRecordingC
  , C.SomeCommand stepTraceBackwardC
  , C.SomeCommand stepTraceForwardC
  , C.SomeCommand initializeSymbolicExecutionC
  , C.SomeCommand beginSymbolicExecutionSetupC
  , C.SomeCommand startSymbolicExecutionC
  , C.SomeCommand setLogFileC
  , C.SomeCommand disableFileLoggingC
  , C.SomeCommand nameValueC
  , C.SomeCommand saveCurrentValueSVGC
  , C.SomeCommand nameCurrentValueC
  ]

exitC :: forall s st . Command s st '[]
exitC =
  C.Command "exit" doc PL.Nil PL.Nil callback (const True)
  where
    doc = text "Cleanly shut down the application and exit"
    callback :: Callback s st '[]
    callback = \customEventChan _ PL.Nil -> SCE.emitEvent customEventChan SCE.Exit

describeCommandC :: forall s st . Command s st '[AR.CommandType]
describeCommandC =
  C.Command "describe-command" doc names rep callback (const True)
  where
    doc = text "Display the docstring of the named command"
    names = C.Const "command-name" PL.:< PL.Nil
    rep = AR.CommandTypeRepr PL.:< PL.Nil
    callback :: Callback s st '[AR.CommandType]
    callback = \customEventChan _ (AR.CommandArgument cmd PL.:< PL.Nil) ->
      SCE.emitEvent customEventChan (SCE.DescribeCommand cmd)

describeKeysC :: forall s st . Command s st '[]
describeKeysC =
  C.Command "describe-keys" doc PL.Nil PL.Nil callback (const True)
  where
    doc = text "Describe the keybindings active in the current mode"
    callback :: Callback s st '[]
    callback = \customEventChan _ PL.Nil ->
      SCE.emitEvent customEventChan SCE.DescribeKeys

nameValueC :: forall s st e u . (st ~ CS.S e u) => Command s st '[AR.ValueNonceType, AR.StringType]
nameValueC =
  C.Command "name-value" doc argNames argTypes callback CS.hasCurrentValue
  where
    doc = text "Provide a name to a distinguished sub-term in a formula inside of the symbolic execution engine"
    argNames = C.Const "nonce" PL.:< C.Const "name" PL.:< PL.Nil
    argTypes = AR.ValueNonceTypeRepr PL.:< AR.StringTypeRepr PL.:< PL.Nil
    callback :: Callback s st '[AR.ValueNonceType, AR.StringType]
    callback = \customEventChan _ (AR.ValueNonceArgument nonce PL.:< AR.StringArgument name PL.:< PL.Nil) ->
      SCE.emitEvent customEventChan (SCE.NameValue nonce name)

nameCurrentValueC :: forall s st e u . (st ~ CS.S e u) => Command s st '[AR.StringType]
nameCurrentValueC =
  C.Command "name-current-value" doc argNames argTypes callback CS.hasCurrentValue
  where
    doc = text "Name a sub-term in the current value (determined by context)"
    callback :: Callback s st '[AR.StringType]
    callback = \customEventChan (AR.getNonce -> AR.SomeNonce archNonce) (AR.StringArgument name PL.:< PL.Nil) ->
      SCE.emitEvent customEventChan (SCE.InitializeValueNamePrompt archNonce name)
    argNames = C.Const "Name" PL.:< PL.Nil
    argTypes = AR.StringTypeRepr PL.:< PL.Nil

selectNextInstructionC :: forall s st . (AR.HasNonce st) => Command s st '[]
selectNextInstructionC =
  C.Command "select-next-instruction" doc PL.Nil PL.Nil callback (const True)
  where
    doc = text "Select the next instruction in the current block viewer"
    callback :: Callback s st '[]
    callback = \customEventChan (AR.getNonce -> AR.SomeNonce archNonce) PL.Nil ->
      SCE.emitEvent customEventChan (SCE.SelectNextInstruction archNonce)

selectPreviousInstructionC :: forall s st . (AR.HasNonce st) => Command s st '[]
selectPreviousInstructionC =
  C.Command "select-previous-instruction" doc PL.Nil PL.Nil callback (const True)
  where
    doc = text "Select the previous instruction in the current block viewer"
    callback :: Callback s st '[]
    callback = \customEventChan (AR.getNonce -> AR.SomeNonce archNonce) PL.Nil ->
      SCE.emitEvent customEventChan (SCE.SelectPreviousInstruction archNonce)

selectNextOperandC :: forall s st . (AR.HasNonce st) => Command s st '[]
selectNextOperandC =
  C.Command "select-next-operand" doc PL.Nil PL.Nil callback (const True)
  where
    doc = text "Select the next operand of the current instruction in the current block viewer"
    callback :: Callback s st '[]
    callback = \customEventChan (AR.getNonce -> AR.SomeNonce archNonce) PL.Nil ->
      SCE.emitEvent customEventChan (SCE.SelectNextOperand archNonce)

selectPreviousOperandC :: forall s st . (AR.HasNonce st) => Command s st '[]
selectPreviousOperandC =
  C.Command "select-previous-operand" doc PL.Nil PL.Nil callback (const True)
  where
    doc = text "Select the previous operand of the current instruction in the current block viewer"
    callback :: Callback s st '[]
    callback = \customEventChan (AR.getNonce -> AR.SomeNonce archNonce) PL.Nil ->
      SCE.emitEvent customEventChan (SCE.SelectPreviousOperand archNonce)

resetInstructionSelectionC :: forall s st . (AR.HasNonce st) => Command s st '[]
resetInstructionSelectionC =
  C.Command "reset-instruction-selection" doc PL.Nil PL.Nil callback (const True)
  where
    doc = text "Clear the selection in the current block viewer"
    callback :: Callback s st '[]
    callback = \customEventChan (AR.getNonce -> AR.SomeNonce archNonce) PL.Nil ->
      SCE.emitEvent customEventChan (SCE.ResetInstructionSelection archNonce)

contextBackC :: forall s st e u . (st ~ CS.S e u) => Command s st '[]
contextBackC =
  C.Command "context-back" doc PL.Nil PL.Nil callback CS.hasContext
  where
    doc = text "Go backward (down) in the context stack"
    callback :: Callback s st '[]
    callback = \customEventChan _ PL.Nil ->
      SCE.emitEvent customEventChan SCE.ContextBack

contextForwardC :: forall s st e u . (st ~ CS.S e u) => Command s st '[]
contextForwardC =
  C.Command "context-forward" doc PL.Nil PL.Nil callback CS.hasContext
  where
    doc = text "Go forward (up) in the context stack"
    callback :: Callback s st '[]
    callback = \customEventChan _ PL.Nil ->
      SCE.emitEvent customEventChan SCE.ContextForward

stepExecutionC :: forall s st e u . (st ~ CS.S e u) => Command s st '[]
stepExecutionC =
  C.Command "step-execution" doc PL.Nil PL.Nil callback CS.hasSuspendedSymbolicExecutionSession
  where
    doc = text "Step execution from the current breakpoint (operates on the current symbolic execution session)"
    callback :: Callback s st '[]
    callback = \customEventChan (AR.SomeState s) PL.Nil ->
      CS.withCurrentSymbolicExecutionSession s (return ()) $ \sessionID ->
        SCE.emitEvent customEventChan (SCE.StepExecution sessionID)

stepOutExecutionC :: forall s st e u . (st ~ CS.S e u) => Command s st '[]
stepOutExecutionC =
  C.Command "step-out-execution" doc PL.Nil PL.Nil callback CS.hasSuspendedSymbolicExecutionSession
  where
    doc = text "Step out of the current function, pausing at the return state"
    callback :: Callback s st '[]
    callback = \customEventChan (AR.SomeState s) PL.Nil ->
      CS.withCurrentSymbolicExecutionSession s (return ()) $ \sessionID ->
        SCE.emitEvent customEventChan (SCE.StepOutExecution sessionID)

interruptExecutionC :: forall s st e u . (st ~ CS.S e u) => Command s st '[]
interruptExecutionC =
  C.Command "interrupt-execution" doc PL.Nil PL.Nil callback CS.hasExecutingSymbolicExecutionSession
  where
    doc = text "Interrupt the current symbolic execution session (operates on the current symbolic execution session)"
    callback :: Callback s st '[]
    callback = \customEventChan (AR.SomeState s) PL.Nil ->
      CS.withCurrentSymbolicExecutionSession s (return ()) $ \sessionID ->
        SCE.emitEvent customEventChan (SCE.InterruptExecution sessionID)

continueExecutionC :: forall s st e u . (st ~ CS.S e u) => Command s st '[]
continueExecutionC =
  C.Command "continue-execution" doc PL.Nil PL.Nil callback CS.hasSuspendedSymbolicExecutionSession
  where
    doc = text "Continue execution from the stopped location (operates on the current symbolic execution session)"
    callback :: Callback s st '[]
    callback = \customEventChan (AR.SomeState s) PL.Nil ->
      CS.withCurrentSymbolicExecutionSession s (return ()) $ \sessionID ->
        SCE.emitEvent customEventChan (SCE.ContinueExecution sessionID)

enableRecordingC :: forall s st e u . (st ~ CS.S e u) => Command s st '[]
enableRecordingC =
  C.Command "enable-recording" doc PL.Nil PL.Nil callback CS.hasSuspendedSymbolicExecutionSession
  where
    doc = text "Start recording symbolic states for replay"
    callback :: Callback s st '[]
    callback = \customEventChan (AR.SomeState s) PL.Nil ->
      CS.withCurrentSymbolicExecutionSession s (return ()) $ \sessionID ->
        SCE.emitEvent customEventChan (SCE.EnableRecording sessionID)

disableRecordingC :: forall s st e u . (st ~ CS.S e u) => Command s st '[]
disableRecordingC =
  C.Command "disable-recording" doc PL.Nil PL.Nil callback CS.hasSuspendedSymbolicExecutionSession
  where
    doc = text "Stop recording symbolic states for replay"
    callback :: Callback s st '[]
    callback = \customEventChan (AR.SomeState s) PL.Nil ->
      CS.withCurrentSymbolicExecutionSession s (return ()) $ \sessionID ->
        SCE.emitEvent customEventChan (SCE.DisableRecording sessionID)

stepTraceBackwardC :: forall s st e u . (st ~ CS.S e u) => Command s st '[]
stepTraceBackwardC =
  C.Command "step-trace-backward" doc PL.Nil PL.Nil callback CS.hasSuspendedSymbolicExecutionSession
  where
    doc = text "Step back in the symbolic trace"
    callback :: Callback s st '[]
    callback = \customEventChan (AR.SomeState s) PL.Nil ->
      CS.withCurrentSymbolicExecutionSession s (return ()) $ \sessionID ->
        SCE.emitEvent customEventChan (SCE.SymbolicStateBack (s ^. CS.lNonce) sessionID)

stepTraceForwardC :: forall s st e u . (st ~ CS.S e u) => Command s st '[]
stepTraceForwardC =
  C.Command "step-trace-forward" doc PL.Nil PL.Nil callback CS.hasSuspendedSymbolicExecutionSession
  where
    doc = text "Step forward in the symbolic trace"
    callback :: Callback s st '[]
    callback = \customEventChan (AR.SomeState s) PL.Nil ->
      CS.withCurrentSymbolicExecutionSession s (return ()) $ \sessionID ->
        SCE.emitEvent customEventChan (SCE.SymbolicStateForward (s ^. CS.lNonce) sessionID)

setLogFileC :: forall s st . Command s st '[AR.FilePathType]
setLogFileC =
  C.Command "set-log-file" doc names rep callback (const True)
  where
    doc = text "Log to a file (in addition to the internal buffer); if the provided filepath is the empty string, the default is used (~/.cache/surveyor.log)"
    names = C.Const "file-name" PL.:< PL.Nil
    rep = AR.FilePathTypeRepr PL.:< PL.Nil
    callback :: Callback s st '[AR.FilePathType]
    callback = \customEventChan _ (AR.FilePathArgument filepath PL.:< PL.Nil) ->
      SCE.emitEvent customEventChan (SCE.SetLogFile filepath)

disableFileLoggingC :: forall s st . Command s st '[]
disableFileLoggingC =
  C.Command "disable-file-logging" doc PL.Nil PL.Nil callback (const True)
  where
    doc = text "Disable logging to a file, if it is enabled"
    callback :: Callback s st '[]
    callback = \customEventChan _ _ ->
      SCE.emitEvent customEventChan SCE.DisableFileLogging

loadFileC :: forall s st . Command s st '[AR.FilePathType]
loadFileC =
  C.Command "load-file" doc names rep callback (const True)
  where
    doc = text "Load a file, attempting to determine its type automatically"
    names = C.Const "file-name" PL.:< PL.Nil
    rep = AR.FilePathTypeRepr PL.:< PL.Nil
    callback :: Callback s st '[AR.FilePathType]
    callback = \customEventChan _ (AR.FilePathArgument filepath PL.:< PL.Nil) ->
      SCE.emitEvent customEventChan (SCE.LoadFile filepath)

loadELFC :: forall s st . Command s st '[AR.FilePathType]
loadELFC =
  C.Command "load-elf" doc names rep callback (const True)
  where
    doc = text "Load an ELF file"
    names = C.Const "file-name" PL.:< PL.Nil
    rep = AR.FilePathTypeRepr PL.:< PL.Nil
    callback :: Callback s st '[AR.FilePathType]
    callback = \customEventChan _ (AR.FilePathArgument filepath PL.:< PL.Nil) ->
      SCE.emitEvent customEventChan (SCE.LoadELF filepath)

loadLLVMC :: forall s st . Command s st '[AR.FilePathType]
loadLLVMC =
  C.Command "load-llvm" doc names rep callback (const True)
  where
    doc = text "Load an LLVM bitcode file"
    names = C.Const "file-name" PL.:< PL.Nil
    rep = AR.FilePathTypeRepr PL.:< PL.Nil
    callback :: Callback s st '[AR.FilePathType]
    callback = \customEventChan _ (AR.FilePathArgument filepath PL.:< PL.Nil) ->
      SCE.emitEvent customEventChan (SCE.LoadLLVM filepath)

loadJARC :: forall s st . Command s st '[AR.FilePathType]
loadJARC =
  C.Command "load-jar" doc names rep callback (const True)
  where
    doc = text "Load a JAR file"
    names = C.Const "file-name" PL.:< PL.Nil
    rep = AR.FilePathTypeRepr PL.:< PL.Nil
    callback :: Callback s st '[AR.FilePathType]
    callback = \customEventChan _ (AR.FilePathArgument filepath PL.:< PL.Nil) ->
      SCE.emitEvent customEventChan (SCE.LoadJAR filepath)

initializeSymbolicExecutionC :: forall s st e u . (st ~ CS.S e u) => Command s st '[]
initializeSymbolicExecutionC =
  C.Command "initialize-symbolic-execution" doc PL.Nil PL.Nil callback CS.hasContext
  where
    doc = text "Initialize symbolic execution for the currently-selected function"
    callback :: Callback s st '[]
    callback = \customEventChan (AR.getNonce -> AR.SomeNonce archNonce) PL.Nil ->
      SCE.emitEvent customEventChan (SCE.InitializeSymbolicExecution archNonce Nothing Nothing)

beginSymbolicExecutionSetupC :: forall s st e u . (st ~ CS.S e u) => Command s st '[]
beginSymbolicExecutionSetupC =
  C.Command "begin-symbolic-execution-setup" doc PL.Nil PL.Nil callback CS.hasContext
  where
    doc = text "Allocate an initial symbolic execution state and prepare it for user customization"
    callback :: Callback s st '[]
    callback customEventChan (AR.SomeState state) PL.Nil
      | nonce <- state ^. CS.lNonce
      , Just archState <- state ^. CS.lArchState
      , Just curCtx <- archState ^? CS.contextL . CCX.currentContext
      , Just (Some symExecState) <- curSymExState state = do
          let fh = curCtx ^. CCX.baseFunctionG
          mcfg <- CA.crucibleCFG (archState ^. CS.lAnalysisResult) fh
          case mcfg of
            Just (CCC.AnyCFG cfg) -> do
              let conf = SymEx.symbolicExecutionConfig symExecState
              SCE.emitEvent customEventChan (SCE.BeginSymbolicExecutionSetup nonce conf (CCC.SomeCFG cfg))
            Nothing ->
              CS.logMessage state (SCL.msgWith { SCL.logText = [text "Missing CFG for function " <> PP.viaShow fh]
                                               , SCL.logLevel = SCL.Warn
                                               , SCL.logSource = SCL.CommandCallback "BeginSymbolicExecution"
                                               })
      | otherwise =
        CS.logMessage state (SCL.msgWith { SCL.logText = [text "Missing context for symbolic execution"]
                                         , SCL.logLevel = SCL.Warn
                                         , SCL.logSource = SCL.CommandCallback "BeginSymbolicExecution"
                                         })

startSymbolicExecutionC :: forall s st e u . (st ~ CS.S e u) => Command s st '[]
startSymbolicExecutionC =
  C.Command "start-symbolic-execution" doc PL.Nil PL.Nil callback isInitializingSymEx
  where
    doc = "Start symbolic execution with the current setup state"
    callback :: Callback s st '[]
    callback customEventChan (AR.SomeState state) PL.Nil
      | nonce <- state ^. CS.lNonce
      , Just (Some (SymEx.Initializing symExecState)) <- curSymExState state
      , Just archState <- state ^. CS.lArchState = do
          let ares = archState ^. CS.lAnalysisResult
          SCE.emitEvent customEventChan (SCE.StartSymbolicExecution nonce ares symExecState)
      | otherwise =
          CS.logMessage state (SCL.msgWith { SCL.logText = [text "Wrong state for starting symbolic execution (expected Initializing)"]
                                           , SCL.logLevel = SCL.Error
                                           , SCL.logSource = SCL.CommandCallback "StartSymbolicExecution"
                                           })
    isInitializingSymEx (AR.SomeState ss) =
      CS.withCurrentSymbolicExecutionSession ss False $ \sessionID ->
        CS.withSymbolicSession ss sessionID False $ \seState ->
          case seState of
            SymEx.Initializing {} -> True
            _ -> False

saveCurrentValueSVGC :: forall s st e u . (st ~ CS.S e u) => Command s st '[AR.FilePathType]
saveCurrentValueSVGC =
  C.Command "save-current-value-svg" doc names rep callback CS.hasCurrentValue
  where
    doc = "Render the current symbolic value as an SVG to the given filename"
    names = C.Const "file-name" PL.:< PL.Nil
    rep = AR.FilePathTypeRepr PL.:< PL.Nil
    callback :: Callback s st '[AR.FilePathType]
    callback = \customEventChan (AR.SomeState ss) (AR.FilePathArgument filePath PL.:< PL.Nil) -> do
      CS.withCurrentSymbolicExecutionSession ss (return ()) $ \sessionID -> do
        CS.withSymbolicSession ss sessionID (return ()) $ \sessionState -> do
          case sessionState of
            SymEx.Suspended _ suspSt
              | Just (Some regEntry) <- SymEx.suspendedCurrentValue suspSt -> do
                  SCE.emitEvent customEventChan (SCE.VisualizeSymbolicTerm regEntry (Just filePath))
            _ -> return ()

curSymExState :: CS.S e u arch s
              -> Maybe (Some (SymEx.SymbolicExecutionState arch s))
curSymExState st
  | Just archState <- st ^. CS.lArchState
  , Just curCtx <- archState ^? CS.contextL . CCX.currentContext
  , sid <- curCtx ^. CCX.symExecSessionIDL =
      SymEx.lookupSessionState (archState ^. CS.symExStateL) sid
  | otherwise = Nothing

-- hasContext :: AR.SomeState (CS.S e u) s -> Bool
-- hasContext (AR.SomeState ss)
--   | Just archState <- ss ^. CS.lArchState =
--       isJust (archState ^? CS.contextL . CCX.currentContext)
--   | otherwise = False

-- withSymbolicSession :: (Monad m)
--                     => CS.S e u arch s
--                     -> SymEx.SessionID s
--                     -> (forall k . SymEx.SymbolicExecutionState arch s k -> m ())
--                     -> m ()
-- withSymbolicSession s sessionID k =
--   case s ^? CS.lArchState . _Just . CS.symExStateL of
--     Nothing -> return ()
--     Just symExSessions ->
--       case SymEx.lookupSessionState symExSessions sessionID of
--         Nothing -> return ()
--         Just (Some symExSt) -> k symExSt

-- withCurrentSymbolicExecutionSession :: (Monad m)
--                                     => CS.S e u arch s
--                                     -> a
--                                     -> (SymEx.SessionID s -> a)
--                                     -> a
-- withCurrentSymbolicExecutionSession s def k =
--   case s ^? CS.lArchState . _Just . CS.contextL . CCX.currentContext . CCX.symExecSessionIDL of
--     Nothing -> def
--     Just sessionID -> k sessionID
