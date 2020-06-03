{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE ViewPatterns #-}
module Surveyor.Brick.Command (
  showMacawBlockC,
  showCrucibleBlockC,
  showBaseBlockC,
  showMacawFunctionC,
  showCrucibleFunctionC,
  showBaseFunctionC,
  showInstructionSemanticsC,
  showSummaryC,
  showDiagnosticsC,
  minibufferC,
  extraCommands,
  findBlockC,
  listFunctionsC,
  mkExtension
  ) where

import qualified Data.Functor.Const as C
import           Data.Kind ( Type )
import qualified Data.Parameterized.List as PL
import qualified Data.Parameterized.Nonce as PN
import           Data.Proxy ( Proxy(..) )
import qualified Data.Text as T

import qualified Surveyor.Brick.Extension as SBE
import           Surveyor.Brick.Names ( Names(..) )
import qualified Surveyor.Brick.Widget.Minibuffer as MB
import qualified Surveyor.Core as C

type Command s st (tps :: [C.ArgumentKind (C.SurveyorCommand s st)]) = C.Command (C.SurveyorCommand s st) tps
type Callback s st (tps :: [C.ArgumentKind (C.SurveyorCommand s st)]) = C.Chan (C.Events s st)
                      -> C.SomeState st s
                      -> PL.List (C.ArgumentType (C.SurveyorCommand s st)) tps
                      -> IO ()

mkExtension :: forall (arch :: Type) s
             . (C.Events s (C.S SBE.BrickUIExtension SBE.BrickUIState) -> IO ())
            -> PN.Nonce s arch
            -> (String -> Maybe (C.SomeAddress s)) -> T.Text -> SBE.BrickUIExtension s
mkExtension emitEvent archNonce addrParser prompt =
  SBE.BrickUIExtension { SBE.sMinibuffer = MB.minibuffer addrParser updater MinibufferEditor MinibufferCompletionList prompt (C.allCommands ++ extraCommands)
                       }
  where
    updater = SBE.updateMinibufferCompletions emitEvent archNonce

extraCommands :: (st ~ C.S SBE.BrickUIExtension SBE.BrickUIState) => [C.SomeCommand (C.SurveyorCommand s st)]
extraCommands = [ C.SomeCommand showMacawBlockC
                , C.SomeCommand showCrucibleBlockC
                , C.SomeCommand showBaseBlockC
                , C.SomeCommand showMacawFunctionC
                , C.SomeCommand showCrucibleFunctionC
                , C.SomeCommand showBaseFunctionC
                , C.SomeCommand showInstructionSemanticsC
                , C.SomeCommand showSummaryC
                , C.SomeCommand showDiagnosticsC
                , C.SomeCommand minibufferC
                , C.SomeCommand findBlockC
                , C.SomeCommand listFunctionsC
                ]



listFunctionsC :: forall s st . (C.HasNonce st, st ~ C.S SBE.BrickUIExtension SBE.BrickUIState) => Command s st '[]
listFunctionsC =
  C.Command "list-functions" doc PL.Nil PL.Nil callback (const True)
  where
    doc = "List all of the discovered functions"
    callback :: Callback s st '[]
    callback = \customEventChan (C.getNonce -> C.SomeNonce nonce) PL.Nil ->
      C.emitEvent customEventChan (SBE.FindFunctionsContaining nonce Nothing)

findBlockC :: forall s st . (C.HasNonce st, st ~ C.S SBE.BrickUIExtension SBE.BrickUIState) => Command s st '[C.AddressType]
findBlockC =
  C.Command "find-block" doc names rep callback (const True)
  where
    doc = "Find the block(s) containing the given address and list them"
    names = C.Const "address" PL.:< PL.Nil
    rep = C.AddressTypeRepr PL.:< PL.Nil
    callback :: Callback s st '[C.AddressType]
    callback = \customEventChan _ (C.AddressArgument (C.SomeAddress anonce addr) PL.:< PL.Nil) ->
      C.emitEvent customEventChan (SBE.FindBlockContaining anonce addr)

showMacawBlockC :: forall s st e u . (C.HasNonce st, st ~ C.S e u) => Command s st '[]
showMacawBlockC =
  C.Command "show-macaw-block" doc PL.Nil PL.Nil callback hasMacawRepr
  where
    doc = "Show the macaw IR of the currently-selected block"
    callback :: Callback s st '[]
    callback = \eventChan sst@(C.SomeState st) PL.Nil -> do
      case C.getNonce sst of
        C.SomeNonce archNonce -> do
          C.logMessage st (C.msgWith { C.logText = ["Showing Macaw IR"]
                                     , C.logLevel = C.Debug
                                     })
          C.emitEvent eventChan (C.ViewBlock archNonce C.MacawRepr)


showCrucibleBlockC :: forall s st e u . (C.HasNonce st, st ~ C.S e u) => Command s st '[]
showCrucibleBlockC =
  C.Command "show-crucible-block" doc PL.Nil PL.Nil callback hasCrucibleRepr
  where
    doc = "Show the crucible IR of the currently-selected block"
    callback :: Callback s st '[]
    callback = \eventChan sst@(C.SomeState st) PL.Nil -> do
      case C.getNonce sst of
        C.SomeNonce archNonce -> do
          C.logMessage st (C.msgWith { C.logText = ["Showing Crucible IR"]
                                     , C.logLevel = C.Debug
                                     })
          C.emitEvent eventChan (C.ViewBlock archNonce C.CrucibleRepr)

showBaseBlockC :: forall s st . (C.HasNonce st) => Command s st '[]
showBaseBlockC =
  C.Command "show-base-block" doc PL.Nil PL.Nil callback (const True)
  where
    doc = "Show the base representation of the currently-selected block"
    callback :: Callback s st '[]
    callback = \eventChan (C.getNonce -> C.SomeNonce archNonce) PL.Nil ->
      C.emitEvent eventChan (C.ViewBlock archNonce C.BaseRepr)

showMacawFunctionC :: forall s st e u . (C.HasNonce st, st ~ C.S e u) => Command s st '[]
showMacawFunctionC =
  C.Command "show-macaw-function" doc PL.Nil PL.Nil callback hasMacawRepr
  where
    doc = "Show the macaw CFG for the currently-selected function"
    callback :: Callback s st '[]
    callback = \eventChan (C.getNonce -> C.SomeNonce archNonce) PL.Nil ->
      C.emitEvent eventChan (C.ViewFunction archNonce C.MacawRepr)

showCrucibleFunctionC :: forall s st e u . (C.HasNonce st, st ~ C.S e u) => Command s st '[]
showCrucibleFunctionC =
  C.Command "show-crucible-function" doc PL.Nil PL.Nil callback hasCrucibleRepr
  where
    doc = "Show the crucible CFG for the currently-selected function"
    callback :: Callback s st '[]
    callback = \eventChan (C.getNonce -> C.SomeNonce archNonce) PL.Nil ->
      C.emitEvent eventChan (C.ViewFunction archNonce C.CrucibleRepr)


showBaseFunctionC :: forall s st . (C.HasNonce st) => Command s st '[]
showBaseFunctionC =
  C.Command "show-base-function" doc PL.Nil PL.Nil callback (const True)
  where
    doc = "Show the base CFG of the currently-selected function"
    callback :: Callback s st '[]
    callback = \eventChan (C.getNonce -> C.SomeNonce archNonce) PL.Nil ->
      C.emitEvent eventChan (C.ViewFunction archNonce C.BaseRepr)

showInstructionSemanticsC :: forall s st . (C.HasNonce st) => Command s st '[]
showInstructionSemanticsC =
  C.Command "show-instruction-semantics" doc PL.Nil PL.Nil callback (const True)
  where
    doc = "Show the semantics for the currently-selected base IR instruction"
    callback :: Callback s st '[]
    callback = \eventChan (C.getNonce -> C.SomeNonce archNonce) PL.Nil ->
      C.emitEvent eventChan (C.ViewInstructionSemantics archNonce)

showSummaryC :: forall s st . (st ~ C.S SBE.BrickUIExtension SBE.BrickUIState) => Command s st '[]
showSummaryC =
  C.Command "summary" doc PL.Nil PL.Nil callback (const True)
  where
    doc = "Show a summary of the information discovered about the binary"
    callback :: Callback s st '[]
    callback = \customEventChan _ PL.Nil -> C.emitEvent customEventChan SBE.ShowSummary

showDiagnosticsC :: forall s st . (st ~ C.S SBE.BrickUIExtension SBE.BrickUIState) => Command s st '[]
showDiagnosticsC =
  C.Command "log" doc PL.Nil PL.Nil callback (const True)
  where
    doc = "Show a log of the diagnostics produced by the analysis and UI"
    callback :: Callback s st '[]
    callback = \customEventChan _ PL.Nil -> C.emitEvent customEventChan SBE.ShowDiagnostics

-- | This isn't part of 'allCommands' because we can never productively launch
-- it from the minibuffer
minibufferC :: forall s st . (st ~ C.S SBE.BrickUIExtension SBE.BrickUIState) => Command s st '[]
minibufferC =
  C.Command "show-minibuffer" doc PL.Nil PL.Nil callback (const True)
  where
    doc = "Open the minibuffer"
    callback :: Callback s st '[]
    callback = \customEventChan _ PL.Nil -> C.emitEvent customEventChan SBE.OpenMinibuffer


hasMacawRepr :: C.SomeState (C.S e u) s -> Bool
hasMacawRepr sst =
  case sst of
    C.SomeState (_ :: C.S e u arch s) ->
      any isMacawRepr (C.alternativeIRs (Proxy @(arch, s)))
  where
    isMacawRepr (C.SomeIRRepr r) =
      case r of
        C.MacawRepr -> True
        _ -> False

hasCrucibleRepr :: C.SomeState (C.S e u) s -> Bool
hasCrucibleRepr sst =
  case sst of
    C.SomeState (_ :: C.S e u arch s) ->
      any isCrucibleRepr (C.alternativeIRs (Proxy @(arch, s)))
  where
    isCrucibleRepr (C.SomeIRRepr r) =
      case r of
        C.CrucibleRepr -> True
        _ -> False

-- FIXME: Should these be in core?
