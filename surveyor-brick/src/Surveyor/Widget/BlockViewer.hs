{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
-- | A widget to view individual basic blocks with fine granularity
--
-- It supports
--
-- * Viewing the instructions in a basic block
-- * Viewing the semantics of an individual instruction for a block
--   (parameterized and instantiated)
-- * Symbolically simulating a range of instructions into a single formula
module Surveyor.Widget.BlockViewer (
  BlockViewer,
  blockViewer,
  blockViewerBlockL,
  handleBlockViewerEvent,
  renderBlockViewer
  ) where

import           GHC.Generics ( Generic )

import qualified Brick as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as B
import           Control.Lens ( Lens', (^.), (&), (.~), (%~), _2, _3, ix, (^?) )
import qualified Data.Generics.Product as GL
import qualified Data.List as L
import           Data.Maybe ( fromMaybe )
import qualified Data.Vector as V
import qualified Graphics.Vty as V
import           Text.Printf ( printf )

import qualified Surveyor.Architecture as A
import           Surveyor.Names ( Names(..) )

data BlockViewer arch s =
  BlockViewer { bvBlock :: !(A.Block arch s)
              , instructionList :: !(B.List Names (A.Address arch s, A.Instruction arch s, OperandSelector arch s))
              }
  deriving (Generic)

instructionListL :: Lens' (BlockViewer arch s) (B.List Names (A.Address arch s, A.Instruction arch s, OperandSelector arch s))
instructionListL = GL.field @"instructionList"

blockViewerBlockL :: Lens' (BlockViewer arch s) (A.Block arch s)
blockViewerBlockL = GL.field @"bvBlock"

blockViewer :: (A.Architecture arch s) => Names -> A.Block arch s -> BlockViewer arch s
blockViewer n b =
  BlockViewer { bvBlock = b
              , instructionList = B.list n (V.fromList insns) 1
              }
  where
    insns = [ (a, i, operandSelector a i)
            | (a, i) <- A.blockInstructions b
            ]

handleBlockViewerEvent :: V.Event -> BlockViewer arch s -> B.EventM Names (BlockViewer arch s)
handleBlockViewerEvent evt bv =
  case evt of
    V.EvKey V.KEsc [] ->
      return $ bv & instructionListL . B.listSelectedL .~ Nothing
    V.EvKey (V.KChar 'n') [V.MCtrl] ->
      case bv ^. instructionListL . B.listSelectedL of
        Nothing -> return $ bv & instructionListL %~ B.listMoveDown
        Just i ->
          return $ bv & instructionListL . B.listElementsL . ix i . _3 %~ operandSelectorReset
                      & instructionListL %~ B.listMoveDown
    V.EvKey V.KDown [] ->
      case bv ^. instructionListL . B.listSelectedL of
        Nothing -> return $ bv & instructionListL %~ B.listMoveDown
        Just i ->
          return $ bv & instructionListL . B.listElementsL . ix i . _3 %~ operandSelectorReset
                      & instructionListL %~ B.listMoveDown
    V.EvKey (V.KChar 'p') [V.MCtrl] ->
      case bv ^. instructionListL . B.listSelectedL of
        Nothing -> return $ bv & instructionListL %~ B.listMoveUp
        Just i ->
          return $ bv & instructionListL . B.listElementsL . ix i . _3 %~ operandSelectorReset
                      & instructionListL %~ B.listMoveUp
    V.EvKey V.KUp [] ->
      case bv ^. instructionListL . B.listSelectedL of
        Nothing -> return $ bv & instructionListL %~ B.listMoveUp
        Just i ->
          return $ bv & instructionListL . B.listElementsL . ix i . _3 %~ operandSelectorReset
                      & instructionListL %~ B.listMoveUp
    V.EvKey V.KLeft [] ->
      case bv ^. instructionListL . B.listSelectedL of
        Nothing -> return bv
        Just i -> return $ bv & instructionListL . B.listElementsL . ix i . _3 %~ operandSelectorPrevious
    V.EvKey V.KRight [] ->
      case bv ^. instructionListL . B.listSelectedL of
        Nothing -> return bv
        Just i -> return $ bv & instructionListL . B.listElementsL . ix i . _3 %~ operandSelectorNext
    _ -> return bv

renderBlockViewer :: (A.Architecture arch s)
                  => A.AnalysisResult arch s
                  -> BlockViewer arch s
                  -> B.Widget Names
renderBlockViewer ares bv =
  B.borderWithLabel header $
  B.hBox [ B.renderList renderListItem False (instructionList bv)
         , B.hLimit 20 semanticsDisplay
         ]
  where
    renderListItem isFocused (addr, _i, os) =
      B.hBox [ B.padRight (B.Pad 2) (B.txt (A.prettyAddress addr))
             , renderOperandSelector isFocused os
             ]
    header =
      let b = bvBlock bv
      in B.str (printf "Basic Block %s" (A.prettyAddress (A.blockAddress b)))
    semanticsDisplay = fromMaybe B.emptyWidget $ do
      selectedIdx <- bv ^. instructionListL . B.listSelectedL
      insn <- bv ^? instructionListL . B.listElementsL . ix selectedIdx . _2
      f <- A.genericSemantics ares insn
      return (B.txtWrap (A.prettyParameterizedFormula f))

-- FIXME: Have a separate semantics display widget.  When a new instruction is
-- selected, send a message to display the semantics, allowing the outer system
-- to place the widget.

operandSelector :: (A.Architecture arch s) => A.Address arch s -> A.Instruction arch s -> OperandSelector arch s
operandSelector addr i =
  OperandSelector { selectedIndex = Nothing
                  , address = addr
                  , operands = V.fromList (A.operands i)
                  , opcode = A.opcode i
                  , boundValue = A.boundValue i
                  }

data OperandSelector arch s =
  OperandSelector { selectedIndex :: Maybe Int
                  -- ^ The index of the selected operand, if any
                  , address :: A.Address arch s
                  -- ^ The address of the instruction rendered on this line
                  , operands :: V.Vector (A.Operand arch s)
                  , opcode :: A.Opcode arch s
                  , boundValue :: Maybe (A.Operand arch s)
                  }
  deriving (Generic)

selectedIndexL :: Lens' (OperandSelector arch s) (Maybe Int)
selectedIndexL = GL.field @"selectedIndex"

renderOperandSelector :: (A.Architecture arch s) => Bool -> OperandSelector arch s -> B.Widget Names
renderOperandSelector isFocused os =
  case selectedIndex os of
    Nothing -> highlight line
    Just _ -> line
  where
    line = B.hBox (pad (B.txt (A.prettyOpcode (opcode os))) : L.intersperse (B.txt ", ") operandWidgets)
    highlight | isFocused = B.withAttr B.listSelectedFocusedAttr
              | otherwise = id
    pad = B.padRight (B.Pad 1)
    renderOperand i op
      | Just i == selectedIndex os =
        highlight (B.txt (A.prettyOperand (address os) op))
      | otherwise = B.txt (A.prettyOperand (address os) op)
    operandWidgets = V.toList (V.imap renderOperand (operands os))

operandSelectorReset :: OperandSelector arch s -> OperandSelector arch s
operandSelectorReset os = os & selectedIndexL .~ Nothing

operandSelectorNext :: OperandSelector arch s -> OperandSelector arch s
operandSelectorNext os =
  os & selectedIndexL %~ maybe (Just 0) f
  where
    f = Just . clamped . (+1)
    clamped = (`min` (V.length (operands os) - 1))

operandSelectorPrevious :: OperandSelector arch s -> OperandSelector arch s
operandSelectorPrevious os =
  os & selectedIndexL %~ maybe (Just lastIndex) f
  where
    lastIndex = V.length (operands os) - 1
    clamped = (`max` 0)
    f = Just . clamped . subtract 1
