{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
-- | A basic function viewer
--
-- The concept is to view a function as a linear stream of blocks with control
-- flow rendered in the margins.
--
-- Ideally, we'll have another view with a more sophisticated CFG type view with
-- a graph layout.
module Surveyor.Brick.Widget.FunctionViewer (
  FunctionViewer,
  functionViewer,
  handleFunctionViewerEvent,
  renderFunctionViewer,
  withConstraints
  ) where

import qualified Brick as B
import qualified Brick.Widgets.List as B
import           Control.DeepSeq ( NFData, rnf )
import           Control.Lens ( (^?), (^.) )
import           Control.Monad.IO.Class ( liftIO )
import qualified Data.Graph.Haggle as H
import qualified Data.Text as T
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Text.Prettyprint.Doc.Render.Text as PPT
import qualified Graphics.Vty as V

import qualified Brick.Widget.Graph as BG
import qualified Surveyor.Core as C
import           Surveyor.Brick.Names ( Names(..) )


data FunctionViewer arch s ir =
  FunctionViewer { fvCallback :: C.FunctionHandle arch s -> C.Block ir s -> IO ()
                 , fvNames :: Names
                 , fvIRRepr :: C.IRRepr arch ir
                 , withConstraints :: forall a . ((C.IR ir s) => a) -> a
                 }

instance NFData (FunctionViewer arch s ir) where
  rnf (FunctionViewer _ !_names !_repr _) = ()

functionViewer :: (C.IR ir s) => (C.FunctionHandle arch s -> C.Block ir s -> IO ()) -> Names -> C.IRRepr arch ir -> (FunctionViewer arch s ir)
functionViewer cb names irrepr =
  FunctionViewer { fvCallback = cb
                 , fvNames = names
                 , fvIRRepr = irrepr
                 , withConstraints = \a -> a
                 }

handleFunctionViewerEvent :: (C.Architecture arch s)
                          => V.Event
                          -> FunctionViewer arch s ir
                          -> C.ContextStack arch s
                          -> B.EventM Names (C.ContextStack arch s)
handleFunctionViewerEvent evt fv cstk =
  case evt of
    V.EvKey V.KDown [] -> return (C.selectNextBlock repr cstk)
    V.EvKey V.KUp [] -> return (C.selectPreviousBlock repr cstk)
    V.EvKey V.KEnter []
      | Just ctx <- cstk ^? C.currentContext
      , Just funcState <- ctx ^. C.functionStateFor repr
      , Just selVert <- funcState ^. C.selectedBlockL
      , Just selBlock <- H.vertexLabel (funcState ^. C.cfgG) selVert -> do
          -- Either send a message (probably put the callback function in the
          -- FunctionViewer constructor) or construct the new context here
          liftIO (fvCallback fv (ctx ^. C.baseFunctionG) selBlock)
          return cstk
      | otherwise -> return cstk
    _ -> return cstk
  where
    repr = fvIRRepr fv

renderFunctionViewer :: (C.Architecture arch s, Eq (C.Address arch s), C.IR ir s)
                     => C.AnalysisResult arch s
                     -> C.ContextStack arch s
                     -> FunctionViewer arch s ir
                     -> B.Widget Names
renderFunctionViewer _ares cstk fv
  | Just ctx <- cstk ^? C.currentContext
  , Just funcState <- ctx ^. C.functionStateFor (fvIRRepr fv) =
      let cfg = funcState ^. C.cfgG
          selectedBlock = funcState ^. C.selectedBlockL
          gr = BG.graph (fvNames fv) cfg selectedBlock 2
      in BG.renderGraph renderNode renderEdge gr
  | otherwise = B.txt (T.pack "No function")

renderNode :: (C.IR arch s) => Bool -> C.Block arch s -> B.Widget Names
renderNode isFocused b =
  xfrm $ B.vBox [ B.txt (C.prettyAddress (C.blockAddress b))
                , B.txt (PPT.renderStrict (PP.layoutCompact ("(" PP.<+> PP.pretty (length (C.blockInstructions b)) PP.<+> ")")))
                ]
  where
    xfrm = if isFocused then B.withAttr B.listSelectedFocusedAttr else id

renderEdge :: () -> B.Widget Names
renderEdge _el = B.emptyWidget
