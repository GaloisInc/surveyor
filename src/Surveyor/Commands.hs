{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  loadJARC,
  loadELFC,
  allCommands
  ) where

import qualified Brick.BChan as B
import qualified Data.Functor.Const as C
import qualified Data.Parameterized.List as PL
import qualified Data.Parameterized.Nonce as NG
import           Data.Parameterized.Some ( Some(..) )

import qualified Brick.Command as C
import qualified Surveyor.Arguments as AR
import           Surveyor.Events ( Events(..) )

type Argument arch s = AR.Argument arch (Events s) (Maybe (NG.Nonce s arch)) s
type Command arch s tps = C.Command (Events s) (Maybe (NG.Nonce s arch)) (Argument arch s) AR.TypeRepr tps
type Callback arch s tps = B.BChan (Events s) -> Maybe (NG.Nonce s arch) -> PL.List (Argument arch s) tps -> IO ()

allCommands :: [Some (C.Command (Events s) (Maybe (NG.Nonce s arch)) (Argument arch s) AR.TypeRepr)]
allCommands =
  [ Some showSummaryC
  , Some exitC
  , Some showDiagnosticsC
  , Some findBlockC
  , Some listFunctionsC
  , Some describeCommandC
  , Some loadFileC
  , Some loadELFC
  , Some loadLLVMC
  , Some loadJARC
  ]

exitC :: forall arch s . Command arch s '[]
exitC =
  C.Command "exit" doc PL.Nil PL.Nil callback
  where
    doc = "Exit the application"
    callback :: Callback arch s '[]
    callback = \customEventChan _ PL.Nil -> B.writeBChan customEventChan Exit

showSummaryC :: forall arch s . Command arch s '[]
showSummaryC =
  C.Command "summary" doc PL.Nil PL.Nil callback
  where
    doc = "Show a summary of the information discovered about the binary"
    callback :: Callback arch s '[]
    callback = \customEventChan _ PL.Nil -> B.writeBChan customEventChan ShowSummary

showDiagnosticsC :: forall arch s . Command arch s '[]
showDiagnosticsC =
  C.Command "log" doc PL.Nil PL.Nil callback
  where
    doc = "Show a log of the diagnostics produced by the analysis and UI"
    callback :: Callback arch s '[]
    callback = \customEventChan _ PL.Nil -> B.writeBChan customEventChan ShowDiagnostics

listFunctionsC :: forall arch s . Command arch s '[]
listFunctionsC =
  C.Command "list-functions" doc PL.Nil PL.Nil callback
  where
    doc = "List all of the discovered functions"
    callback :: Callback arch s '[]
    callback = \customEventChan mnonce PL.Nil ->
      case mnonce of
        Nothing -> return ()
        Just nonce ->
          B.writeBChan customEventChan (FindFunctionsContaining nonce Nothing)

findBlockC :: forall arch s . Command arch s '[AR.AddressType]
findBlockC =
  C.Command "find-block" doc names rep callback
  where
    doc = "Find the block(s) containing the given address and list them"
    names = C.Const "address" PL.:< PL.Nil
    rep = AR.AddressTypeRepr PL.:< PL.Nil
    callback :: Callback arch s '[AR.AddressType]
    callback = \customEventChan mnonce (AR.AddressArgument addr PL.:< PL.Nil) ->
      case mnonce of
        Nothing -> return ()
        Just nonce ->
          B.writeBChan customEventChan (FindBlockContaining nonce addr)

describeCommandC :: forall arch s . Command arch s '[AR.CommandType]
describeCommandC =
  C.Command "describe-command" doc names rep callback
  where
    doc = "Display the docstring of the named command"
    names = C.Const "command-name" PL.:< PL.Nil
    rep = AR.CommandTypeRepr PL.:< PL.Nil
    callback :: Callback arch s '[AR.CommandType]
    callback = \customEventChan _ (AR.CommandArgument cmd PL.:< PL.Nil) ->
      B.writeBChan customEventChan (DescribeCommand cmd)

-- | This isn't part of 'allCommands' because we can never productively launch
-- it from the minibuffer
minibufferC :: forall arch s . Command arch s '[]
minibufferC =
  C.Command "show-minibuffer" doc PL.Nil PL.Nil callback
  where
    doc = "Open the minibuffer"
    callback :: Callback arch s '[]
    callback = \customEventChan _ PL.Nil -> B.writeBChan customEventChan OpenMinibuffer

loadFileC :: forall arch s . Command arch s '[AR.FilePathType]
loadFileC =
  C.Command "load-file" doc names rep callback
  where
    doc = "Load a file, attempting to determine its type automatically"
    names = C.Const "file-name" PL.:< PL.Nil
    rep = AR.FilePathTypeRepr PL.:< PL.Nil
    callback :: Callback arch s '[AR.FilePathType]
    callback = \customEventChan _ (AR.FilePathArgument filepath PL.:< PL.Nil) ->
      B.writeBChan customEventChan (LoadFile filepath)

loadELFC :: forall arch s . Command arch s '[AR.FilePathType]
loadELFC =
  C.Command "load-elf" doc names rep callback
  where
    doc = "Load an ELF file"
    names = C.Const "file-name" PL.:< PL.Nil
    rep = AR.FilePathTypeRepr PL.:< PL.Nil
    callback :: Callback arch s '[AR.FilePathType]
    callback = \customEventChan _ (AR.FilePathArgument filepath PL.:< PL.Nil) ->
      B.writeBChan customEventChan (LoadELF filepath)

loadLLVMC :: forall arch s . Command arch s '[AR.FilePathType]
loadLLVMC =
  C.Command "load-llvm" doc names rep callback
  where
    doc = "Load an LLVM bitcode file"
    names = C.Const "file-name" PL.:< PL.Nil
    rep = AR.FilePathTypeRepr PL.:< PL.Nil
    callback :: Callback arch s '[AR.FilePathType]
    callback = \customEventChan _ (AR.FilePathArgument filepath PL.:< PL.Nil) ->
      B.writeBChan customEventChan (LoadLLVM filepath)

loadJARC :: forall arch s . Command arch s '[AR.FilePathType]
loadJARC =
  C.Command "load-jar" doc names rep callback
  where
    doc = "Load a JAR file"
    names = C.Const "file-name" PL.:< PL.Nil
    rep = AR.FilePathTypeRepr PL.:< PL.Nil
    callback :: Callback arch s '[AR.FilePathType]
    callback = \customEventChan _ (AR.FilePathArgument filepath PL.:< PL.Nil) ->
      B.writeBChan customEventChan (LoadJAR filepath)
