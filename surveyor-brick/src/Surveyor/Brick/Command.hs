{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}
module Surveyor.Brick.Command (
  showMacawBlockC,
  showCrucibleBlockC,
  showBaseBlockC,
  showInstructionSemanticsC,
  extraCommands
  ) where

import qualified Data.Parameterized.List as PL

import qualified Surveyor.Core as C

type Command s st (tps :: [C.ArgumentKind (C.SurveyorCommand s st)]) = C.Command (C.SurveyorCommand s st) tps
type Callback s st (tps :: [C.ArgumentKind (C.SurveyorCommand s st)]) = C.Chan (C.Events s st)
                      -> Maybe (C.SomeNonce s)
                      -> PL.List (C.ArgumentType (C.SurveyorCommand s st)) tps
                      -> IO ()

extraCommands :: [C.SomeCommand (C.SurveyorCommand s st)]
extraCommands = [ C.SomeCommand showMacawBlockC
                , C.SomeCommand showCrucibleBlockC
                , C.SomeCommand showBaseBlockC
                , C.SomeCommand showInstructionSemanticsC
                ]

showMacawBlockC :: forall s t . Command s t '[]
showMacawBlockC =
  C.Command "show-macaw-block" doc PL.Nil PL.Nil callback
  where
    doc = "Show the macaw IR of the currently-selected block"
    callback :: Callback s st '[]
    callback = \eventChan mNonce PL.Nil ->
      case mNonce of
        Nothing -> return ()
        Just (C.SomeNonce archNonce) -> do
          C.writeChan eventChan (C.LogDiagnostic (Just C.LogDebug) "Showing macaw IR")
          C.writeChan eventChan (C.ViewBlock archNonce C.MacawRepr)

showCrucibleBlockC :: forall s t . Command s t '[]
showCrucibleBlockC =
  C.Command "show-crucible-block" doc PL.Nil PL.Nil callback
  where
    doc = "Show the crucible IR of the currently-selected block"
    callback :: Callback s st '[]
    callback = \eventChan mNonce PL.Nil ->
      case mNonce of
        Nothing -> return ()
        Just (C.SomeNonce archNonce) -> do
          C.writeChan eventChan (C.LogDiagnostic (Just C.LogDebug) "Showing crucible IR")
          C.writeChan eventChan (C.ViewBlock archNonce C.CrucibleRepr)

showBaseBlockC :: forall s t . Command s t '[]
showBaseBlockC =
  C.Command "show-base-block" doc PL.Nil PL.Nil callback
  where
    doc = "Show the base representation of the currently-selected block"
    callback :: Callback s st '[]
    callback = \eventChan mNonce PL.Nil ->
      case mNonce of
        Nothing -> return ()
        Just (C.SomeNonce archNonce) ->
          C.writeChan eventChan (C.ViewBlock archNonce C.BaseRepr)

showInstructionSemanticsC :: forall s t . Command s t '[]
showInstructionSemanticsC =
  C.Command "show-instruction-semantics" doc PL.Nil PL.Nil callback
  where
    doc = "Show the semantics for the currently-selected base IR instruction"
    callback :: Callback s st '[]
    callback = \eventChan mNonce PL.Nil ->
      case mNonce of
        Nothing -> return ()
        Just (C.SomeNonce archNonce) ->
          C.writeChan eventChan (C.ViewInstructionSemantics archNonce)
