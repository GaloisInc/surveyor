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
  continueExecutionC,
  interruptExecutionC,
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

import           Control.Lens ( (^.), (^?), _Just )
import qualified Data.Functor.Const as C
import           Data.Maybe ( isJust )
import qualified Data.Parameterized.List as PL
import           Data.Parameterized.Some ( Some(..) )
import           Fmt ( (+||), (||+) )
import qualified Fmt as Fmt
import qualified Lang.Crucible.CFG.Core as CCC

import qualified Surveyor.Core.Architecture as CA
import qualified Surveyor.Core.Arguments as AR
import qualified Surveyor.Core.Chan as C
import qualified Surveyor.Core.Command as C
import qualified Surveyor.Core.Context as CCX
import qualified Surveyor.Core.Events as SCE
import qualified Surveyor.Core.Logging as SCL
import qualified Surveyor.Core.State as CS
import qualified Surveyor.Core.SymbolicExecution as SymEx

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
  , C.SomeCommand stepExecutionC
  , C.SomeCommand continueExecutionC
  , C.SomeCommand interruptExecutionC
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
    doc = "Exit the application"
    callback :: Callback s st '[]
    callback = \customEventChan _ PL.Nil -> SCE.emitEvent customEventChan SCE.Exit


describeCommandC :: forall s st . Command s st '[AR.CommandType]
describeCommandC =
  C.Command "describe-command" doc names rep callback (const True)
  where
    doc = "Display the docstring of the named command"
    names = C.Const "command-name" PL.:< PL.Nil
    rep = AR.CommandTypeRepr PL.:< PL.Nil
    callback :: Callback s st '[AR.CommandType]
    callback = \customEventChan _ (AR.CommandArgument cmd PL.:< PL.Nil) ->
      SCE.emitEvent customEventChan (SCE.DescribeCommand cmd)

describeKeysC :: forall s st . Command s st '[]
describeKeysC =
  C.Command "describe-keys" doc PL.Nil PL.Nil callback (const True)
  where
    doc = "Describe the keybindings active in the current mode"
    callback :: Callback s st '[]
    callback = \customEventChan _ PL.Nil ->
      SCE.emitEvent customEventChan SCE.DescribeKeys

nameValueC :: forall s st . Command s st '[AR.ValueNonceType, AR.StringType]
nameValueC =
  C.Command "name-value" doc argNames argTypes callback hasCurrentValue
  where
    doc = "Name a sub-term in a formula"
    argNames = C.Const "nonce" PL.:< C.Const "name" PL.:< PL.Nil
    argTypes = AR.ValueNonceTypeRepr PL.:< AR.StringTypeRepr PL.:< PL.Nil
    callback :: Callback s st '[AR.ValueNonceType, AR.StringType]
    callback = \customEventChan _ (AR.ValueNonceArgument nonce PL.:< AR.StringArgument name PL.:< PL.Nil) ->
      SCE.emitEvent customEventChan (SCE.NameValue nonce name)
    -- FIXME: Maybe this is always available?  Or perhaps never available for
    -- interactive use because the user can never provide a nonce via prompt
    hasCurrentValue = const True

nameCurrentValueC :: forall s st e u . (st ~ CS.S e u) => Command s st '[AR.StringType]
nameCurrentValueC =
  C.Command "name-current-value" doc argNames argTypes callback hasCurrentValue
  where
    doc = "Name a sub-term in the current value (determined by context)"
    callback :: Callback s st '[AR.StringType]
    callback = \customEventChan (AR.getNonce -> AR.SomeNonce archNonce) (AR.StringArgument name PL.:< PL.Nil) ->
      SCE.emitEvent customEventChan (SCE.InitializeValueNamePrompt archNonce name)
    hasCurrentValue :: AR.SomeState (CS.S e u) s -> Bool
    hasCurrentValue (AR.SomeState st)
      | Just sessionID <- st ^? CS.lArchState . _Just . CS.contextL . CCX.currentContext . CCX.symExecSessionIDL
      , Just symExSt <- st ^? CS.lArchState . _Just . CS.symExStateL
      , Just (Some (SymEx.Suspended _symNonce suspSt)) <- SymEx.lookupSessionState symExSt sessionID =
          isJust (SymEx.suspendedCurrentValue suspSt)
      | otherwise = False
    argNames = C.Const "Name" PL.:< PL.Nil
    argTypes = AR.StringTypeRepr PL.:< PL.Nil

selectNextInstructionC :: forall s st . (AR.HasNonce st) => Command s st '[]
selectNextInstructionC =
  C.Command "select-next-instruction" doc PL.Nil PL.Nil callback (const True)
  where
    doc = "Select the next instruction in the current block viewer"
    callback :: Callback s st '[]
    callback = \customEventChan (AR.getNonce -> AR.SomeNonce archNonce) PL.Nil ->
      SCE.emitEvent customEventChan (SCE.SelectNextInstruction archNonce)

selectPreviousInstructionC :: forall s st . (AR.HasNonce st) => Command s st '[]
selectPreviousInstructionC =
  C.Command "select-previous-instruction" doc PL.Nil PL.Nil callback (const True)
  where
    doc = "Select the previous instruction in the current block viewer"
    callback :: Callback s st '[]
    callback = \customEventChan (AR.getNonce -> AR.SomeNonce archNonce) PL.Nil ->
      SCE.emitEvent customEventChan (SCE.SelectPreviousInstruction archNonce)

selectNextOperandC :: forall s st . (AR.HasNonce st) => Command s st '[]
selectNextOperandC =
  C.Command "select-next-operand" doc PL.Nil PL.Nil callback (const True)
  where
    doc = "Select the next operand of the current instruction in the current block viewer"
    callback :: Callback s st '[]
    callback = \customEventChan (AR.getNonce -> AR.SomeNonce archNonce) PL.Nil ->
      SCE.emitEvent customEventChan (SCE.SelectNextOperand archNonce)

selectPreviousOperandC :: forall s st . (AR.HasNonce st) => Command s st '[]
selectPreviousOperandC =
  C.Command "select-previous-operand" doc PL.Nil PL.Nil callback (const True)
  where
    doc = "Select the previous operand of the current instruction in the current block viewer"
    callback :: Callback s st '[]
    callback = \customEventChan (AR.getNonce -> AR.SomeNonce archNonce) PL.Nil ->
      SCE.emitEvent customEventChan (SCE.SelectPreviousOperand archNonce)

resetInstructionSelectionC :: forall s st . (AR.HasNonce st) => Command s st '[]
resetInstructionSelectionC =
  C.Command "reset-instruction-selection" doc PL.Nil PL.Nil callback (const True)
  where
    doc = "Clear the selection in the current block viewer"
    callback :: Callback s st '[]
    callback = \customEventChan (AR.getNonce -> AR.SomeNonce archNonce) PL.Nil ->
      SCE.emitEvent customEventChan (SCE.ResetInstructionSelection archNonce)

contextBackC :: forall s st . Command s st '[]
contextBackC =
  C.Command "context-back" doc PL.Nil PL.Nil callback (const True)
  where
    doc = "Go backward (down) in the context stack"
    callback :: Callback s st '[]
    callback = \customEventChan _ PL.Nil ->
      SCE.emitEvent customEventChan SCE.ContextBack

contextForwardC :: forall s st . Command s st '[]
contextForwardC =
  C.Command "context-forward" doc PL.Nil PL.Nil callback (const True)
  where
    doc = "Go forward (up) in the context stack"
    callback :: Callback s st '[]
    callback = \customEventChan _ PL.Nil ->
      SCE.emitEvent customEventChan SCE.ContextForward

stepExecutionC :: forall s st e u . (st ~ CS.S e u) => Command s st '[]
stepExecutionC =
  C.Command "step-execution" doc PL.Nil PL.Nil callback (const True)
  where
    doc = "Step execution from the current breakpoint (operates on the current symbolic execution session)"
    callback :: Callback s st '[]
    callback = \customEventChan (AR.SomeState s) PL.Nil ->
      withCurrentSymbolicExecutionSession s $ \sessionID ->
        SCE.emitEvent customEventChan (SCE.StepExecution sessionID)

interruptExecutionC :: forall s st e u . (st ~ CS.S e u) => Command s st '[]
interruptExecutionC =
  C.Command "interrupt-execution" doc PL.Nil PL.Nil callback (const True)
  where
    doc = "Interrupt the current symbolic execution session (operates on the current symbolic execution session)"
    callback :: Callback s st '[]
    callback = \customEventChan (AR.SomeState s) PL.Nil ->
      withCurrentSymbolicExecutionSession s $ \sessionID ->
        SCE.emitEvent customEventChan (SCE.InterruptExecution sessionID)

continueExecutionC :: forall s st e u . (st ~ CS.S e u) => Command s st '[]
continueExecutionC =
  C.Command "continue-execution" doc PL.Nil PL.Nil callback (const True)
  where
    doc = "Continue execution from the stopped location (operates on the current symbolic execution session)"
    callback :: Callback s st '[]
    callback = \customEventChan (AR.SomeState s) PL.Nil ->
      withCurrentSymbolicExecutionSession s $ \sessionID ->
        SCE.emitEvent customEventChan (SCE.ContinueExecution sessionID)

setLogFileC :: forall s st . Command s st '[AR.FilePathType]
setLogFileC =
  C.Command "set-log-file" doc names rep callback (const True)
  where
    doc = "Log to a file (in addition to the internal buffer); if the provided filepath is the empty string, the default is used (~/.cache/surveyor.log)"
    names = C.Const "file-name" PL.:< PL.Nil
    rep = AR.FilePathTypeRepr PL.:< PL.Nil
    callback :: Callback s st '[AR.FilePathType]
    callback = \customEventChan _ (AR.FilePathArgument filepath PL.:< PL.Nil) ->
      SCE.emitEvent customEventChan (SCE.SetLogFile filepath)

disableFileLoggingC :: forall s st . Command s st '[]
disableFileLoggingC =
  C.Command "disable-file-logging" doc PL.Nil PL.Nil callback (const True)
  where
    doc = "Disable logging to a file, if it is enabled"
    callback :: Callback s st '[]
    callback = \customEventChan _ _ ->
      SCE.emitEvent customEventChan SCE.DisableFileLogging

loadFileC :: forall s st . Command s st '[AR.FilePathType]
loadFileC =
  C.Command "load-file" doc names rep callback (const True)
  where
    doc = "Load a file, attempting to determine its type automatically"
    names = C.Const "file-name" PL.:< PL.Nil
    rep = AR.FilePathTypeRepr PL.:< PL.Nil
    callback :: Callback s st '[AR.FilePathType]
    callback = \customEventChan _ (AR.FilePathArgument filepath PL.:< PL.Nil) ->
      SCE.emitEvent customEventChan (SCE.LoadFile filepath)

loadELFC :: forall s st . Command s st '[AR.FilePathType]
loadELFC =
  C.Command "load-elf" doc names rep callback (const True)
  where
    doc = "Load an ELF file"
    names = C.Const "file-name" PL.:< PL.Nil
    rep = AR.FilePathTypeRepr PL.:< PL.Nil
    callback :: Callback s st '[AR.FilePathType]
    callback = \customEventChan _ (AR.FilePathArgument filepath PL.:< PL.Nil) ->
      SCE.emitEvent customEventChan (SCE.LoadELF filepath)

loadLLVMC :: forall s st . Command s st '[AR.FilePathType]
loadLLVMC =
  C.Command "load-llvm" doc names rep callback (const True)
  where
    doc = "Load an LLVM bitcode file"
    names = C.Const "file-name" PL.:< PL.Nil
    rep = AR.FilePathTypeRepr PL.:< PL.Nil
    callback :: Callback s st '[AR.FilePathType]
    callback = \customEventChan _ (AR.FilePathArgument filepath PL.:< PL.Nil) ->
      SCE.emitEvent customEventChan (SCE.LoadLLVM filepath)

loadJARC :: forall s st . Command s st '[AR.FilePathType]
loadJARC =
  C.Command "load-jar" doc names rep callback (const True)
  where
    doc = "Load a JAR file"
    names = C.Const "file-name" PL.:< PL.Nil
    rep = AR.FilePathTypeRepr PL.:< PL.Nil
    callback :: Callback s st '[AR.FilePathType]
    callback = \customEventChan _ (AR.FilePathArgument filepath PL.:< PL.Nil) ->
      SCE.emitEvent customEventChan (SCE.LoadJAR filepath)

initializeSymbolicExecutionC :: forall s st e u . (st ~ CS.S e u) => Command s st '[]
initializeSymbolicExecutionC =
  C.Command "initialize-symbolic-execution" doc PL.Nil PL.Nil callback hasContext
  where
    doc = "Initialize symbolic execution for the currently-selected function"
    callback :: Callback s st '[]
    callback = \customEventChan (AR.getNonce -> AR.SomeNonce archNonce) PL.Nil ->
      SCE.emitEvent customEventChan (SCE.InitializeSymbolicExecution archNonce Nothing Nothing)

beginSymbolicExecutionSetupC :: forall s st e u . (st ~ CS.S e u) => Command s st '[]
beginSymbolicExecutionSetupC =
  C.Command "begin-symbolic-execution-setup" doc PL.Nil PL.Nil callback hasContext
  where
    doc = "Allocate an initial symbolic execution state and prepare it for user customization"
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
              CS.logMessage state (SCL.msgWith { SCL.logText = [Fmt.fmt ("Missing CFG for function "+||fh||+"")]
                                               , SCL.logLevel = SCL.Warn
                                               , SCL.logSource = SCL.CommandCallback "BeginSymbolicExecution"
                                               })
      | otherwise =
        CS.logMessage state (SCL.msgWith { SCL.logText = ["Missing context for symbolic execution"]
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
          CS.logMessage state (SCL.msgWith { SCL.logText = ["Wrong state for starting symbolic execution"]
                                           , SCL.logLevel = SCL.Error
                                           , SCL.logSource = SCL.CommandCallback "StartSymbolicExecution"
                                           })
    isInitializingSymEx (AR.SomeState ss)
      | Just (Some (SymEx.Initializing {})) <- curSymExState ss = True
      | otherwise = False

saveCurrentValueSVGC :: forall s st e u . (st ~ CS.S e u) => Command s st '[AR.FilePathType]
saveCurrentValueSVGC =
  C.Command "save-current-value-svg" doc names rep callback (const True)
  where
    doc = "Render the current symbolic value as an SVG to the given filename"
    names = C.Const "file-name" PL.:< PL.Nil
    rep = AR.FilePathTypeRepr PL.:< PL.Nil
    callback :: Callback s st '[AR.FilePathType]
    callback = \customEventChan (AR.SomeState ss) (AR.FilePathArgument filePath PL.:< PL.Nil) -> do
      withCurrentSymbolicExecutionSession ss $ \sessionID -> do
        withSymbolicSession ss sessionID $ \sessionState -> do
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

hasContext :: AR.SomeState (CS.S e u) s -> Bool
hasContext (AR.SomeState ss)
  | Just archState <- ss ^. CS.lArchState =
      isJust (archState ^? CS.contextL . CCX.currentContext)
  | otherwise = False

withSymbolicSession :: (Monad m)
                    => CS.S e u arch s
                    -> SymEx.SessionID s
                    -> (forall k . SymEx.SymbolicExecutionState arch s k -> m ())
                    -> m ()
withSymbolicSession s sessionID k =
  case s ^? CS.lArchState . _Just . CS.symExStateL of
    Nothing -> return ()
    Just symExSessions ->
      case SymEx.lookupSessionState symExSessions sessionID of
        Nothing -> return ()
        Just (Some symExSt) -> k symExSt

withCurrentSymbolicExecutionSession :: (Monad m)
                                    => CS.S e u arch s
                                    -> (SymEx.SessionID s -> m ())
                                    -> m ()
withCurrentSymbolicExecutionSession s k =
  case s ^? CS.lArchState . _Just . CS.contextL . CCX.currentContext . CCX.symExecSessionIDL of
    Nothing -> return ()
    Just sessionID -> k sessionID
