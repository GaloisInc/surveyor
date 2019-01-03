{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | A widget for displaying the semantics for an individual instruction
--
-- This is intended for displaying machine code instruction semantics.  It
-- currently has no interactivity, so there is no event handler.
module Surveyor.Brick.Widget.InstructionSemanticsViewer (
  InstructionSemanticsViewer,
  instructionSemanticsViewer,
  renderInstructionSemanticsViewer
  ) where

import qualified Brick as B
import           Control.DeepSeq ( NFData, rnf )
import           Control.Lens ( (^?), (^.) )
import qualified Data.Vector as V
import qualified Fmt as Fmt
import           Fmt ( (+|), (|+) )

import qualified Surveyor.Core as C
import           Surveyor.Brick.Names ( Names(..) )

-- The rendering only requires a view of the context, so there is no data
-- payload for the widget itself (for now)
data InstructionSemanticsViewer arch s =
  InstructionSemanticsViewer

instance NFData (InstructionSemanticsViewer arch s) where
  rnf !_ = ()

instructionSemanticsViewer :: InstructionSemanticsViewer arch s
instructionSemanticsViewer = InstructionSemanticsViewer

-- | Render the instructions for the selected instruction (of the base
-- architecture) if there is a single selected instruction *and* there are
-- semantics available for it
renderInstructionSemanticsViewer :: (C.Architecture arch s)
                                 => C.AnalysisResult arch s
                                 -> C.ContextStack arch s
                                 -> InstructionSemanticsViewer arch s
                                 -> B.Widget Names
renderInstructionSemanticsViewer ares cs _
  | Just ctx <- cs ^? C.currentContext
  , Just blkState <- ctx ^. C.blockStateFor C.BaseRepr =
      case blkState ^. C.blockStateSelection of
        C.NoSelection -> B.txt "No selected instruction"
        C.MultipleSelection {} -> B.txt "Multiple instructions selected"
        C.SingleSelection ix _addr _ ->
          let insns = blkState ^. C.blockStateList
          in case insns V.!? ix of
            Nothing -> B.txt (Fmt.fmt ("ERROR: Instruction index out of range: " +| ix |+ ""))
            Just (_, addr, i) ->
              case C.genericSemantics ares i of
                Nothing -> B.txt (Fmt.fmt ("No semantics for instruction: " +| C.prettyInstruction addr i |+ ""))
                Just sem -> B.txt (C.prettyParameterizedFormula sem)
  | otherwise = B.txt "No current block"
