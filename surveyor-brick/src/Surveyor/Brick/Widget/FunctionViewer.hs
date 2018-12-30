{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
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
  renderFunctionViewer
  ) where

import           GHC.Generics ( Generic )

import qualified Brick as B
import qualified Brick.Widgets.List as B
import           Control.DeepSeq ( NFData, rnf )
import           Control.Lens ( (^?), (^.) )
import           Control.Monad.IO.Class ( liftIO )
import qualified Data.Graph.Haggle as H
import qualified Data.Text as T
import qualified Fmt as Fmt
import           Fmt ( (+|), (|+) )
import qualified Graphics.Vty as V

import qualified Brick.Widget.Graph as BG
import qualified Surveyor.Core as C
import           Surveyor.Brick.Names ( Names(..) )


data FunctionViewer arch s = FunctionViewer (C.Block arch s -> IO ()) Names
  deriving (Generic)

instance NFData (FunctionViewer arch s) where
  rnf (FunctionViewer _ !_names) = ()

functionViewer :: (C.Block arch s -> IO ()) -> Names -> (FunctionViewer arch s)
functionViewer = FunctionViewer

handleFunctionViewerEvent :: (C.Architecture arch s)
                          => V.Event
                          -> FunctionViewer arch s
                          -> C.ContextStack arch s
                          -> B.EventM Names (C.ContextStack arch s)
handleFunctionViewerEvent evt (FunctionViewer selectBlock _name) cstk =
  case evt of
    V.EvKey V.KDown [] -> return (C.selectNextBlock cstk)
    V.EvKey V.KUp [] -> return (C.selectPreviousBlock cstk)
    V.EvKey V.KEnter []
      | Just ctx <- cstk ^? C.currentContext
      , Just selVert <- ctx ^. C.selectedBlockL
      , Just selBlock <- H.vertexLabel (ctx ^. C.cfgG) selVert -> do
          -- Either send a message (probably put the callback function in the
          -- FunctionViewer constructor) or construct the new context here
          liftIO (selectBlock selBlock)
          return cstk
      | otherwise -> return cstk
    _ -> return cstk

renderFunctionViewer :: (C.Architecture arch s, Eq (C.Address arch s))
                     => C.AnalysisResult arch s
                     -> C.ContextStack arch s
                     -> FunctionViewer arch s
                     -> B.Widget Names
renderFunctionViewer _ares cstk (FunctionViewer _ names)
  | Just ctx <- cstk ^? C.currentContext =
      let cfg = ctx ^. C.cfgG
          selectedBlock = ctx ^. C.selectedBlockL
          gr = BG.graph names cfg selectedBlock 2
      in BG.renderGraph renderNode renderEdge gr
  | otherwise = B.txt (T.pack "No function")

renderNode :: (C.IR arch s) => Bool -> C.Block arch s -> B.Widget Names
renderNode isFocused b =
  xfrm $ B.vBox [ B.txt (C.prettyAddress (C.blockAddress b))
                , B.txt (Fmt.fmt ("("+|length (C.blockInstructions b)|+")"))
                ]
  where
    xfrm = if isFocused then B.withAttr B.listSelectedFocusedAttr else id

renderEdge :: () -> B.Widget Names
renderEdge _el = B.emptyWidget
