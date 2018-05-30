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
module Surveyor.Brick.Widget.BlockViewer (
  BlockViewer,
  emptyBlockViewer,
  blockViewer,
  blockViewerBlockL,
  asBlockViewer,
  handleBlockViewerEvent,
  renderBlockViewer
  ) where

import           GHC.Generics ( Generic )

import qualified Brick as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as B
import           Control.Lens ( Prism', Lens', (^.), (&), (.~), (%~), _2, _3, ix, (^?) )
import qualified Data.Generics.Product as GL
import qualified Data.Generics.Sum as GS
import qualified Data.List as L
import           Data.Maybe ( fromMaybe )
import qualified Data.Vector as V
import qualified Graphics.Vty as V
import           Text.Printf ( printf )

import qualified Surveyor.Core as C
import           Surveyor.Brick.Names ( Names(..) )

data BlockViewer arch s = NoBlock
                        | BlockViewer (MkBlockViewer arch s)
                        deriving (Generic)

data MkBlockViewer arch s =
  MkBlockViewer { bvBlock :: !(C.Block arch s)
                , instructionList :: !(B.List Names (C.Address arch s, C.Instruction arch s, OperandSelector arch s))
                }
  deriving (Generic)

asBlockViewer :: Prism' (BlockViewer arch s) (MkBlockViewer arch s)
asBlockViewer = GS._Ctor @"BlockViewer"

instructionListL :: Lens' (MkBlockViewer arch s) (B.List Names (C.Address arch s, C.Instruction arch s, OperandSelector arch s))
instructionListL = GL.field @"instructionList"

blockViewerBlockL :: Lens' (MkBlockViewer arch s) (C.Block arch s)
blockViewerBlockL = GL.field @"bvBlock"

-- I really want this, but can't figure out the type:
--
-- > realBlockViewerL = asBlockViewer . blockViewerBlockL

emptyBlockViewer :: BlockViewer arch s
emptyBlockViewer = NoBlock

blockViewer :: (C.Architecture arch s) => Names -> C.Block arch s -> BlockViewer arch s
blockViewer n b =
  BlockViewer MkBlockViewer { bvBlock = b
                            , instructionList = B.list n (V.fromList insns) 1
                            }
  where
    insns = [ (a, i, operandSelector a i)
            | (a, i) <- C.blockInstructions b
            ]

handleBlockViewerEvent :: V.Event -> BlockViewer arch s -> B.EventM Names (BlockViewer arch s)
handleBlockViewerEvent _ NoBlock = return NoBlock
handleBlockViewerEvent evt (BlockViewer bv) =
  case evt of
    V.EvKey V.KEsc [] ->
      return $ BlockViewer $ bv & instructionListL . B.listSelectedL .~ Nothing
    V.EvKey (V.KChar 'n') [V.MCtrl] ->
      case bv ^. instructionListL . B.listSelectedL of
        Nothing -> return $ BlockViewer $ bv & instructionListL %~ B.listMoveDown
        Just i ->
          return $ BlockViewer $ bv & instructionListL . B.listElementsL . ix i . _3 %~ operandSelectorReset
                                    & instructionListL %~ B.listMoveDown
    V.EvKey V.KDown [] ->
      case bv ^. instructionListL . B.listSelectedL of
        Nothing -> return $ BlockViewer $ bv & instructionListL %~ B.listMoveDown
        Just i ->
          return $ BlockViewer $ bv & instructionListL . B.listElementsL . ix i . _3 %~ operandSelectorReset
                                    & instructionListL %~ B.listMoveDown
    V.EvKey (V.KChar 'p') [V.MCtrl] ->
      case bv ^. instructionListL . B.listSelectedL of
        Nothing -> return $ BlockViewer $ bv & instructionListL %~ B.listMoveUp
        Just i ->
          return $ BlockViewer $ bv & instructionListL . B.listElementsL . ix i . _3 %~ operandSelectorReset
                                    & instructionListL %~ B.listMoveUp
    V.EvKey V.KUp [] ->
      case bv ^. instructionListL . B.listSelectedL of
        Nothing -> return $ BlockViewer $ bv & instructionListL %~ B.listMoveUp
        Just i ->
          return $ BlockViewer $ bv & instructionListL . B.listElementsL . ix i . _3 %~ operandSelectorReset
                                    & instructionListL %~ B.listMoveUp
    V.EvKey V.KLeft [] ->
      case bv ^. instructionListL . B.listSelectedL of
        Nothing -> return (BlockViewer bv)
        Just i -> return $ BlockViewer $ bv & instructionListL . B.listElementsL . ix i . _3 %~ operandSelectorPrevious
    V.EvKey V.KRight [] ->
      case bv ^. instructionListL . B.listSelectedL of
        Nothing -> return (BlockViewer bv)
        Just i -> return $ BlockViewer $ bv & instructionListL . B.listElementsL . ix i . _3 %~ operandSelectorNext
    _ -> return (BlockViewer bv)

renderBlockViewer :: (C.Architecture arch s)
                  => C.AnalysisResult arch s
                  -> BlockViewer arch s
                  -> B.Widget Names
renderBlockViewer _ NoBlock = B.txt "No block"
renderBlockViewer ares (BlockViewer bv) =
  B.borderWithLabel header $
  B.hBox [ B.renderList renderListItem False (instructionList bv)
         , B.hLimit 20 semanticsDisplay
         ]
  where
    renderListItem isFocused (addr, _i, os) =
      B.hBox [ B.padRight (B.Pad 2) (B.txt (C.prettyAddress addr))
             , renderOperandSelector isFocused os
             ]
    header =
      let b = bvBlock bv
      in B.str (printf "Basic Block %s" (C.prettyAddress (C.blockAddress b)))
    semanticsDisplay = fromMaybe B.emptyWidget $ do
      selectedIdx <- bv ^. instructionListL . B.listSelectedL
      insn <- bv ^? instructionListL . B.listElementsL . ix selectedIdx . _2
      f <- C.genericSemantics ares insn
      return (B.txtWrap (C.prettyParameterizedFormula f))

-- FIXME: Have a separate semantics display widget.  When a new instruction is
-- selected, send a message to display the semantics, allowing the outer system
-- to place the widget.

operandSelector :: (C.Architecture arch s) => C.Address arch s -> C.Instruction arch s -> OperandSelector arch s
operandSelector addr i =
  OperandSelector { selectedIndex = Nothing
                  , address = addr
                  , operands = V.fromList (C.operands i)
                  , opcode = C.opcode i
                  , boundValue = C.boundValue i
                  }

data OperandSelector arch s =
  OperandSelector { selectedIndex :: Maybe Int
                  -- ^ The index of the selected operand, if any
                  , address :: C.Address arch s
                  -- ^ The address of the instruction rendered on this line
                  , operands :: V.Vector (C.Operand arch s)
                  , opcode :: C.Opcode arch s
                  , boundValue :: Maybe (C.Operand arch s)
                  }
  deriving (Generic)

selectedIndexL :: Lens' (OperandSelector arch s) (Maybe Int)
selectedIndexL = GL.field @"selectedIndex"

renderOperandSelector :: (C.Architecture arch s) => Bool -> OperandSelector arch s -> B.Widget Names
renderOperandSelector isFocused os =
  case selectedIndex os of
    Nothing -> highlight line
    Just _ -> line
  where
    line = B.hBox (pad (B.txt (C.prettyOpcode (opcode os))) : L.intersperse (B.txt ", ") operandWidgets)
    highlight | isFocused = B.withAttr B.listSelectedFocusedAttr
              | otherwise = id
    pad = B.padRight (B.Pad 1)
    renderOperand i op
      | Just i == selectedIndex os =
        highlight (B.txt (C.prettyOperand (address os) op))
      | otherwise = B.txt (C.prettyOperand (address os) op)
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
