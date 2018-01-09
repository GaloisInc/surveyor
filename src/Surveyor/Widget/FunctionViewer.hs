-- | A basic function viewer
--
-- The concept is to view a function as a linear stream of blocks with control
-- flow rendered in the margins.
--
-- Ideally, we'll have another view with a more sophisticated CFG type view with
-- a graph layout.
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module Surveyor.Widget.FunctionViewer (
  FunctionViewer,
  functionViewer,
  handleFunctionViewerEvent,
  renderFunctionViewer
  ) where

import           GHC.Generics ( Generic )

import qualified Brick as B
import           Control.Lens ( Lens' )
import qualified Data.Foldable as F
import qualified Data.Generics.Product as GL
import qualified Data.Map.Strict as M
import qualified Graphics.Vty as V

import qualified Surveyor.Architecture as A
import           Surveyor.Names ( Names )

data FunctionViewer arch s =
  FunctionViewer { funcHandle :: A.FunctionHandle arch s
                 -- ^ A pointer to the function being displayed
                 , analysisResult :: A.AnalysisResult arch s
                 -- ^ The analysis result to find blocks in
                 , funcBlocks :: [A.Block arch s]
                 -- ^ The blocks in the function
                 , focusedBlock :: Maybe (A.Address arch s)
                 -- ^ The currently-focused block, if any
                 , blockMap :: M.Map (A.Address arch s) (A.Block arch s)
                 -- ^ A mapping of addresses to blocks so that we can figure out
                 -- which block is focused
                 }
  deriving (Generic)

focusedBlockL :: Lens' (FunctionViewer arch s) (Maybe (A.Address arch s))
focusedBlockL = GL.field @"focusedBlock"

functionViewer :: (Ord (A.Address arch s), A.Architecture arch s)
               => A.FunctionHandle arch s
               -> A.AnalysisResult arch s
               -> FunctionViewer arch s
functionViewer fh ar =
  FunctionViewer { funcHandle = fh
                 , analysisResult = ar
                 , funcBlocks = blocks
                 , focusedBlock = Nothing
                 , blockMap = F.foldl' indexBlock M.empty blocks
                 }
  where
    blocks = A.functionBlocks ar fh
    indexBlock m b = M.insert (A.blockAddress b) b m

handleFunctionViewerEvent :: (A.Architecture arch s)
                          => V.Event
                          -> FunctionViewer arch s
                          -> B.EventM Names (FunctionViewer arch s)
handleFunctionViewerEvent = undefined

renderFunctionViewer :: (A.Architecture arch s)
                     => FunctionViewer arch s
                     -> B.Widget Names
renderFunctionViewer = undefined
