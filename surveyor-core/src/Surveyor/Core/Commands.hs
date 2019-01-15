{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Surveyor.Core.Commands (
  exitC,
  showSummaryC,
  showDiagnosticsC,
  findBlockC,
  listFunctionsC,
  describeCommandC,
  minibufferC,
  loadFileC,
  loadLLVMC,
  loadJARC,
  loadELFC,
  selectNextInstructionC,
  selectPreviousInstructionC,
  selectNextOperandC,
  selectPreviousOperandC,
  resetInstructionSelectionC,
  allCommands
  ) where

import qualified Data.Functor.Const as C
import qualified Data.Parameterized.List as PL

import qualified Surveyor.Core.Arguments as AR
import qualified Surveyor.Core.Chan as C
import qualified Surveyor.Core.Command as C
import           Surveyor.Core.Events ( Events(..) )


type Command s st tps = C.Command (AR.SurveyorCommand s st) tps
type Callback s st tps = C.Chan (Events s st)
                      -> Maybe (AR.SomeNonce s)
                      -> PL.List (C.ArgumentType (AR.SurveyorCommand s st)) tps
                      -> IO ()

allCommands :: [C.SomeCommand (AR.SurveyorCommand s st)]
allCommands =
  [ C.SomeCommand showSummaryC
  , C.SomeCommand exitC
  , C.SomeCommand showDiagnosticsC
  , C.SomeCommand findBlockC
  , C.SomeCommand listFunctionsC
  , C.SomeCommand describeCommandC
  , C.SomeCommand loadFileC
  , C.SomeCommand loadELFC
  , C.SomeCommand loadLLVMC
  , C.SomeCommand loadJARC
  , C.SomeCommand selectNextInstructionC
  , C.SomeCommand selectPreviousInstructionC
  , C.SomeCommand selectNextOperandC
  , C.SomeCommand selectPreviousOperandC
  , C.SomeCommand resetInstructionSelectionC
  ]

exitC :: forall s st . Command s st '[]
exitC =
  C.Command "exit" doc PL.Nil PL.Nil callback
  where
    doc = "Exit the application"
    callback :: Callback s st '[]
    callback = \customEventChan _ PL.Nil -> C.writeChan customEventChan Exit

showSummaryC :: forall s st . Command s st '[]
showSummaryC =
  C.Command "summary" doc PL.Nil PL.Nil callback
  where
    doc = "Show a summary of the information discovered about the binary"
    callback :: Callback s st '[]
    callback = \customEventChan _ PL.Nil -> C.writeChan customEventChan ShowSummary

showDiagnosticsC :: forall s st . Command s st '[]
showDiagnosticsC =
  C.Command "log" doc PL.Nil PL.Nil callback
  where
    doc = "Show a log of the diagnostics produced by the analysis and UI"
    callback :: Callback s st '[]
    callback = \customEventChan _ PL.Nil -> C.writeChan customEventChan ShowDiagnostics

listFunctionsC :: forall s st . Command s st '[]
listFunctionsC =
  C.Command "list-functions" doc PL.Nil PL.Nil callback
  where
    doc = "List all of the discovered functions"
    callback :: Callback s st '[]
    callback = \customEventChan mnonce PL.Nil ->
      case mnonce of
        Nothing -> return ()
        Just (AR.SomeNonce nonce) ->
          C.writeChan customEventChan (FindFunctionsContaining nonce Nothing)

findBlockC :: forall s st . Command s st '[AR.AddressType]
findBlockC =
  C.Command "find-block" doc names rep callback
  where
    doc = "Find the block(s) containing the given address and list them"
    names = C.Const "address" PL.:< PL.Nil
    rep = AR.AddressTypeRepr PL.:< PL.Nil
    callback :: Callback s st '[AR.AddressType]
    callback = \customEventChan mnonce (AR.AddressArgument (AR.SomeAddress anonce addr) PL.:< PL.Nil) ->
      case mnonce of
        Nothing -> return ()
        Just (AR.SomeNonce _nonce) ->
            C.writeChan customEventChan (FindBlockContaining anonce addr)

describeCommandC :: forall s st . Command s st '[AR.CommandType]
describeCommandC =
  C.Command "describe-command" doc names rep callback
  where
    doc = "Display the docstring of the named command"
    names = C.Const "command-name" PL.:< PL.Nil
    rep = AR.CommandTypeRepr PL.:< PL.Nil
    callback :: Callback s st '[AR.CommandType]
    callback = \customEventChan _ (AR.CommandArgument cmd PL.:< PL.Nil) ->
      C.writeChan customEventChan (DescribeCommand cmd)

-- | This isn't part of 'allCommands' because we can never productively launch
-- it from the minibuffer
minibufferC :: forall s st . Command s st '[]
minibufferC =
  C.Command "show-minibuffer" doc PL.Nil PL.Nil callback
  where
    doc = "Open the minibuffer"
    callback :: Callback s st '[]
    callback = \customEventChan _ PL.Nil -> C.writeChan customEventChan OpenMinibuffer

selectNextInstructionC :: forall s st . Command s st '[]
selectNextInstructionC =
  C.Command "select-next-instruction" doc PL.Nil PL.Nil callback
  where
    doc = "Select the next instruction in the current block viewer"
    callback :: Callback s st '[]
    callback = \customEventChan mnonce PL.Nil ->
      case mnonce of
        Nothing -> return ()
        Just (AR.SomeNonce archNonce) ->
          C.writeChan customEventChan (SelectNextInstruction archNonce)

selectPreviousInstructionC :: forall s st . Command s st '[]
selectPreviousInstructionC =
  C.Command "select-previous-instruction" doc PL.Nil PL.Nil callback
  where
    doc = "Select the previous instruction in the current block viewer"
    callback :: Callback s st '[]
    callback = \customEventChan mnonce PL.Nil ->
      case mnonce of
        Nothing -> return ()
        Just (AR.SomeNonce archNonce) ->
          C.writeChan customEventChan (SelectPreviousInstruction archNonce)

selectNextOperandC :: forall s st . Command s st '[]
selectNextOperandC =
  C.Command "select-next-operand" doc PL.Nil PL.Nil callback
  where
    doc = "Select the next operand of the current instruction in the current block viewer"
    callback :: Callback s st '[]
    callback = \customEventChan mnonce PL.Nil ->
      case mnonce of
        Nothing -> return ()
        Just (AR.SomeNonce archNonce) ->
          C.writeChan customEventChan (SelectNextOperand archNonce)

selectPreviousOperandC :: forall s st . Command s st '[]
selectPreviousOperandC =
  C.Command "select-previous-operand" doc PL.Nil PL.Nil callback
  where
    doc = "Select the previous operand of the current instruction in the current block viewer"
    callback :: Callback s st '[]
    callback = \customEventChan mnonce PL.Nil ->
      case mnonce of
        Nothing -> return ()
        Just (AR.SomeNonce archNonce) ->
          C.writeChan customEventChan (SelectPreviousOperand archNonce)

resetInstructionSelectionC :: forall s st . Command s st '[]
resetInstructionSelectionC =
  C.Command "reset-instruction-selection" doc PL.Nil PL.Nil callback
  where
    doc = "Clear the selection in the current block viewer"
    callback :: Callback s st '[]
    callback = \customEventChan mnonce PL.Nil ->
      case mnonce of
        Nothing -> return ()
        Just (AR.SomeNonce archNonce) ->
          C.writeChan customEventChan (ResetInstructionSelection archNonce)

loadFileC :: forall s st . Command s st '[AR.FilePathType]
loadFileC =
  C.Command "load-file" doc names rep callback
  where
    doc = "Load a file, attempting to determine its type automatically"
    names = C.Const "file-name" PL.:< PL.Nil
    rep = AR.FilePathTypeRepr PL.:< PL.Nil
    callback :: Callback s st '[AR.FilePathType]
    callback = \customEventChan _ (AR.FilePathArgument filepath PL.:< PL.Nil) ->
      C.writeChan customEventChan (LoadFile filepath)

loadELFC :: forall s st . Command s st '[AR.FilePathType]
loadELFC =
  C.Command "load-elf" doc names rep callback
  where
    doc = "Load an ELF file"
    names = C.Const "file-name" PL.:< PL.Nil
    rep = AR.FilePathTypeRepr PL.:< PL.Nil
    callback :: Callback s st '[AR.FilePathType]
    callback = \customEventChan _ (AR.FilePathArgument filepath PL.:< PL.Nil) ->
      C.writeChan customEventChan (LoadELF filepath)

loadLLVMC :: forall s st . Command s st '[AR.FilePathType]
loadLLVMC =
  C.Command "load-llvm" doc names rep callback
  where
    doc = "Load an LLVM bitcode file"
    names = C.Const "file-name" PL.:< PL.Nil
    rep = AR.FilePathTypeRepr PL.:< PL.Nil
    callback :: Callback s st '[AR.FilePathType]
    callback = \customEventChan _ (AR.FilePathArgument filepath PL.:< PL.Nil) ->
      C.writeChan customEventChan (LoadLLVM filepath)

loadJARC :: forall s st . Command s st '[AR.FilePathType]
loadJARC =
  C.Command "load-jar" doc names rep callback
  where
    doc = "Load a JAR file"
    names = C.Const "file-name" PL.:< PL.Nil
    rep = AR.FilePathTypeRepr PL.:< PL.Nil
    callback :: Callback s st '[AR.FilePathType]
    callback = \customEventChan _ (AR.FilePathArgument filepath PL.:< PL.Nil) ->
      C.writeChan customEventChan (LoadJAR filepath)
