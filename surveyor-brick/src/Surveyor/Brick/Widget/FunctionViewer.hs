{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
-- | A basic function viewer
--
-- The concept is to view a function as a linear stream of blocks with control
-- flow rendered in the margins.
--
-- Ideally, we'll have another view with a more sophisticated CFG type view with
-- a graph layout.
module Surveyor.Brick.Widget.FunctionViewer (
  FunctionViewer,
  asFunctionViewer,
  emptyFunctionViewer,
  functionViewer,
  handleFunctionViewerEvent,
  renderFunctionViewer
  ) where

import           GHC.Generics ( Generic )

import qualified Brick as B
import           Control.DeepSeq ( NFData, rnf, deepseq )
import           Control.Lens ( Lens', Prism', (^.), (^?) )
import qualified Data.Foldable as F
import qualified Data.Generics.Product as GL
import qualified Data.Generics.Sum as GS
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Graphics.Vty as V

import qualified Surveyor.Core as C
import           Surveyor.Brick.Names ( Names(..) )
import qualified Surveyor.Brick.Widget.BlockViewer as BV

data FunctionViewer arch s = NoFunction
                           | FunctionViewer (MkFunctionViewer arch s)
                           deriving (Generic)

instance (C.ArchConstraints arch s) => NFData (FunctionViewer arch s)

data MkFunctionViewer arch s =
  MkFunctionViewer { funcHandle :: C.FunctionHandle arch s
                 -- ^ A pointer to the function being displayed
                 , analysisResult :: C.AnalysisResult arch s
                 -- ^ The analysis result to find blocks in
                 , funcBlocks :: [BV.BlockViewer arch s]
                 -- ^ The blocks in the function
                 , focusedBlock :: Maybe (C.Address arch s)
                 -- ^ The currently-focused block, if any
                 , blockMap :: M.Map (C.Address arch s) (C.Block arch s)
                 -- ^ A mapping of addresses to blocks so that we can figure out
                 -- which block is focused
                 }
  deriving (Generic)

instance (C.ArchConstraints arch s) => NFData (MkFunctionViewer arch s) where
  rnf mk = funcHandle mk `deepseq`
           funcBlocks mk `deepseq`
           focusedBlock mk `deepseq`
           blockMap mk `deepseq` ()

asFunctionViewer :: Prism' (FunctionViewer arch s) (MkFunctionViewer arch s)
asFunctionViewer = GS._Ctor @"FunctionViewer"

focusedBlockL :: Lens' (MkFunctionViewer arch s) (Maybe (C.Address arch s))
focusedBlockL = GL.field @"focusedBlock"

emptyFunctionViewer :: FunctionViewer arch s
emptyFunctionViewer = NoFunction

functionViewer :: (Ord (C.Address arch s), C.Architecture arch s)
               => C.FunctionHandle arch s
               -> C.AnalysisResult arch s
               -> FunctionViewer arch s
functionViewer fh ar =
  FunctionViewer MkFunctionViewer { funcHandle = fh
                                  , analysisResult = ar
                                  , funcBlocks = blockViewers
                                  , focusedBlock = Nothing
                                  , blockMap = F.foldl' indexBlock M.empty blocks
                                  }
  where
    identifiers = map BlockViewerList [0..]
    blockViewers = map (uncurry BV.blockViewer) (zip identifiers (L.sortOn C.blockAddress blocks))
    blocks = C.functionBlocks ar fh
    indexBlock m b = M.insert (C.blockAddress b) b m

handleFunctionViewerEvent :: (C.Architecture arch s)
                          => V.Event
                          -> FunctionViewer arch s
                          -> B.EventM Names (FunctionViewer arch s)
handleFunctionViewerEvent _ NoFunction = return NoFunction
handleFunctionViewerEvent _ fv = return fv

renderFunctionViewer :: (C.Architecture arch s, Eq (C.Address arch s))
                     => FunctionViewer arch s
                     -> B.Widget Names
renderFunctionViewer NoFunction = B.txt (T.pack "No function")
renderFunctionViewer (FunctionViewer fv) = blockList
--  B.viewport FunctionViewport B.Both blockList
  where
    blockList = B.vBox (map renderWithFocus (funcBlocks fv))
    renderWithFocus mb =
      case mb ^? BV.asBlockViewer of
        Nothing -> B.emptyWidget
        Just b ->
          let w = B.padBottom (B.Pad 1) (BV.renderBlockViewer (analysisResult fv) mb)
          in case fv ^. focusedBlockL == Just (C.blockAddress (b ^. BV.blockViewerBlockL)) of
            True -> B.visible w
            False -> w
