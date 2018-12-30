{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Surveyor.Brick.Command (
  showMacawBlockC,
  showBaseBlockC,
  showInstructionSemanticsC,
  extraCommands
  ) where

import qualified Data.Parameterized.List as PL
import           Data.Parameterized.Some ( Some(..) )

import qualified Surveyor.Core as C

type Argument s st = C.Argument (C.Events s st) (Maybe (C.SomeNonce s)) s
type Command s st tps = C.Command (C.Events s st) (Maybe (C.SomeNonce s)) (Argument s st) C.TypeRepr tps
type Callback s st tps = C.Chan (C.Events s st) -> Maybe (C.SomeNonce s) -> PL.List (Argument s st) tps -> IO ()

extraCommands :: [Some (C.Command (C.Events s st) (Maybe (C.SomeNonce s)) (Argument s st) C.TypeRepr)]
extraCommands = [ Some showMacawBlockC
                , Some showBaseBlockC
                , Some showInstructionSemanticsC
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
