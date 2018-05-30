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
  allCommands,
  SomeNonce(..)
  ) where

import qualified Data.Functor.Const as C
import qualified Data.Parameterized.List as PL
import qualified Data.Parameterized.Nonce as NG
import           Data.Parameterized.Some ( Some(..) )

import qualified Surveyor.Core.Arguments as AR
import qualified Surveyor.Core.Chan as C
import qualified Surveyor.Core.Command as C
import           Surveyor.Core.Events ( Events(..) )

-- | This is a separate wrapper (instead of the Some from parameterized-utils)
-- because we want to constrain it with a kind signature.
data SomeNonce s where
  SomeNonce :: forall (arch :: *) s . NG.Nonce s arch -> SomeNonce s

type Argument s st = AR.Argument (Events s st) (Maybe (SomeNonce s)) s
type Command s st tps = C.Command (Events s st) (Maybe (SomeNonce s)) (Argument s st) AR.TypeRepr tps
type Callback s st tps = C.Chan (Events s st) -> Maybe (SomeNonce s) -> PL.List (Argument s st) tps -> IO ()

allCommands :: [Some (C.Command (Events s st) (Maybe (SomeNonce s)) (Argument s st) AR.TypeRepr)]
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
        Just (SomeNonce nonce) ->
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
        Just (SomeNonce _nonce) ->
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
