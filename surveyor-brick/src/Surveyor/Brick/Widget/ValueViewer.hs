{-# LANGUAGE GADTs #-}
-- | A viewer for Crucible run-time values (RegEntries)
module Surveyor.Brick.Widget.ValueViewer (
  ValueViewer,
  valueViewer,
  renderValueViewer
  ) where

import qualified Brick as B
import qualified Data.Functor.Const as C
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.Nonce as PN
import qualified Lang.Crucible.Simulator.RegMap as CSR

import           Surveyor.Brick.Names ( Names(..) )

data ValueViewerState s sym tp =
  ValueViewerState { regEntry :: CSR.RegEntry sym tp
                   , cache :: MapF.MapF (PN.Nonce s) (C.Const (B.Widget Names))
                   -- ^ Previous translations of terms into widgets, cached to
                   -- avoid recomputation
                   }

data ValueViewer s where
  ValueViewer :: ValueViewerState s sym tp -> ValueViewer s

valueViewer :: CSR.RegEntry sym tp -> ValueViewer s
valueViewer re = ValueViewer (ValueViewerState re MapF.empty)

renderValueViewer :: ValueViewer s -> B.Widget Names
renderValueViewer vv = B.vBox []
