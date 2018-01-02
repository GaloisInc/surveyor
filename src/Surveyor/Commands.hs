{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
module Surveyor.Commands (
  exitC,
  showSummaryC,
  showDiagnosticsC,
  findBlockC,
  listFunctionsC,
  describeCommandC,
  minibufferC,
  loadFileC,
  loadLLVMC,
  loadELFC,
  allCommands
  ) where

import qualified Brick.BChan as B
import qualified Data.Functor.Const as C
import qualified Data.Parameterized.List as PL
import           Data.Parameterized.Some ( Some(..) )

import qualified Brick.Command as C
import qualified Surveyor.Arguments as AR
import           Surveyor.Events ( Events(..) )
import           Surveyor.State

allCommands :: B.BChan (Events s) -> [Some (C.Command (S arch s) (AR.Argument arch (S arch s) s) AR.TypeRepr)]
allCommands customEventChan =
  [ Some (showSummaryC customEventChan)
  , Some (exitC customEventChan)
  , Some (showDiagnosticsC customEventChan)
  , Some (findBlockC customEventChan)
  , Some (listFunctionsC customEventChan)
  , Some (describeCommandC customEventChan)
  , Some (loadFileC customEventChan)
  , Some (loadELFC customEventChan)
  , Some (loadLLVMC customEventChan)
  ]

exitC :: B.BChan (Events s) -> C.Command (S arch s) (AR.Argument arch (S arch s) s) AR.TypeRepr '[]
exitC customEventChan =
  C.Command "exit" doc PL.Nil PL.Nil callback
  where
    doc = "Exit the application"
    callback = \_ PL.Nil -> B.writeBChan customEventChan Exit

showSummaryC :: B.BChan (Events s) -> C.Command (S arch s) (AR.Argument arch (S arch s) s) AR.TypeRepr '[]
showSummaryC customEventChan =
  C.Command "summary" doc PL.Nil PL.Nil callback
  where
    doc = "Show a summary of the information discovered about the binary"
    callback = \_ PL.Nil -> B.writeBChan customEventChan ShowSummary

showDiagnosticsC :: B.BChan (Events s) -> C.Command (S arch s) (AR.Argument arch (S arch s) s) AR.TypeRepr '[]
showDiagnosticsC customEventChan =
  C.Command "log" doc PL.Nil PL.Nil callback
  where
    doc = "Show a log of the diagnostics produced by the analysis and UI"
    callback = \_ PL.Nil -> B.writeBChan customEventChan ShowDiagnostics

listFunctionsC :: B.BChan (Events s) -> C.Command (S arch s) (AR.Argument arch (S arch s) s) AR.TypeRepr '[]
listFunctionsC customEventChan =
  C.Command "list-functions" doc PL.Nil PL.Nil callback
  where
    doc = "List all of the discovered functions"
    callback = \st PL.Nil -> B.writeBChan customEventChan (FindFunctionsContaining (sNonce (sArchState st)) Nothing)

findBlockC :: B.BChan (Events s) -> C.Command (S arch s) (AR.Argument arch (S arch s) s) AR.TypeRepr '[AR.AddressType]
findBlockC customEventChan =
  C.Command "find-block" doc names rep callback
  where
    doc = "Find the block(s) containing the given address and list them"
    names = C.Const "address" PL.:< PL.Nil
    rep = AR.AddressTypeRepr PL.:< PL.Nil
    callback = \st (AR.AddressArgument addr PL.:< PL.Nil) ->
      B.writeBChan customEventChan (FindBlockContaining (sNonce (sArchState st)) addr)

describeCommandC :: B.BChan (Events s) -> C.Command (S arch s) (AR.Argument arch (S arch s) s) AR.TypeRepr '[AR.CommandType]
describeCommandC customEventChan =
  C.Command "describe-command" doc names rep callback
  where
    doc = "Display the docstring of the named command"
    names = C.Const "command-name" PL.:< PL.Nil
    rep = AR.CommandTypeRepr PL.:< PL.Nil
    callback = \_ (AR.CommandArgument cmd PL.:< PL.Nil) ->
      B.writeBChan customEventChan (DescribeCommand cmd)

-- | This isn't part of 'allCommands' because we can never productively launch
-- it from the minibuffer
minibufferC :: B.BChan (Events s) -> C.Command (S arch s) (AR.Argument arch (S arch s) s) AR.TypeRepr '[]
minibufferC customEventChan =
  C.Command "show-minibuffer" doc PL.Nil PL.Nil callback
  where
    doc = "Open the minibuffer"
    callback = \_ PL.Nil -> B.writeBChan customEventChan OpenMinibuffer

loadFileC :: B.BChan (Events s) -> C.Command (S arch s) (AR.Argument arch (S arch s) s) AR.TypeRepr '[AR.FilePathType]
loadFileC customEventChan =
  C.Command "load-file" doc names rep callback
  where
    doc = "Load a file, attempting to determine its type automatically"
    names = C.Const "file-name" PL.:< PL.Nil
    rep = AR.FilePathTypeRepr PL.:< PL.Nil
    callback = \_ (AR.FilePathArgument filepath PL.:< PL.Nil) ->
      B.writeBChan customEventChan (LoadFile filepath)

loadELFC :: B.BChan (Events s) -> C.Command (S arch s) (AR.Argument arch (S arch s) s) AR.TypeRepr '[AR.FilePathType]
loadELFC customEventChan =
  C.Command "load-elf" doc names rep callback
  where
    doc = "Load an ELF file"
    names = C.Const "file-name" PL.:< PL.Nil
    rep = AR.FilePathTypeRepr PL.:< PL.Nil
    callback = \_ (AR.FilePathArgument filepath PL.:< PL.Nil) ->
      B.writeBChan customEventChan (LoadELF filepath)

loadLLVMC :: B.BChan (Events s) -> C.Command (S arch s) (AR.Argument arch (S arch s) s) AR.TypeRepr '[AR.FilePathType]
loadLLVMC customEventChan =
  C.Command "load-llvm" doc names rep callback
  where
    doc = "Load an LLVM bitcode file"
    names = C.Const "file-name" PL.:< PL.Nil
    rep = AR.FilePathTypeRepr PL.:< PL.Nil
    callback = \_ (AR.FilePathArgument filepath PL.:< PL.Nil) ->
      B.writeBChan customEventChan (LoadLLVM filepath)
