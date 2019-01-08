{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  blockViewer,
  handleBlockViewerEvent,
  renderBlockViewer
  ) where

import qualified Brick as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as B
import           Control.DeepSeq ( NFData, rnf )
import           Control.Lens ( (^.), (^?), (&), (.~) )
import qualified Data.ByteString as BS
import qualified Data.List as L
import           Data.Maybe ( isJust )
import           Data.Parameterized.Classes
import           Data.Proxy ( Proxy(..) )
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Fmt as Fmt
import           Fmt ( (+|), (|+) )
import qualified Graphics.Vty as V
import           Text.Printf ( printf )

import qualified Surveyor.Core as C
import           Surveyor.Brick.Names ( Names(..) )


data BlockViewer arch s ir where
  BlockViewer :: (C.Architecture arch s, C.IR ir s) => !Names -> !(C.IRRepr arch ir) -> BlockViewer arch s ir

instance NFData (BlockViewer arch s ir) where
  rnf (BlockViewer !_names !_repr) = ()

-- | We want to put as much of the state as we can in the shared Context, then
-- lazily update the per-block viewers as they actually need it (i.e., during
-- rendering).  The problem here is that the renderer will have to re-build the
-- list backing store each time it is invoked.  This might not be an issue -
-- only the selection would need to be modified, which is minimal.
--
-- The alternative is to eagerly update all block viewers every time an event is
-- handled (i.e., have the event handler return all of the updated block
-- viewers).  This would mean that the lists would have to be part of the
-- context.  Updating them all at once would have the added benefit of
-- potentially making it possible to statically enforce that the top of the
-- context stack matches the state.
blockViewer :: (C.Architecture arch s, C.IR ir s) => Names -> C.IRRepr arch ir -> BlockViewer arch s ir
blockViewer names repr = BlockViewer names repr

handleBlockViewerEvent :: V.Event -> BlockViewer arch s ir -> C.ContextStack arch s -> B.EventM Names (C.ContextStack arch s)
handleBlockViewerEvent evt (BlockViewer _ repr) cstk =
  case evt of
    V.EvKey V.KEsc [] -> return (C.resetBlockSelection cstk)
    V.EvKey V.KDown [] -> return (C.selectNextInstruction repr cstk)
    V.EvKey (V.KChar 'n') [V.MCtrl] -> return (C.selectNextInstruction repr cstk)
    V.EvKey V.KUp [] -> return (C.selectPreviousInstruction repr cstk)
    V.EvKey (V.KChar 'p') [V.MCtrl] -> return (C.selectPreviousInstruction repr cstk)
    V.EvKey V.KRight [] -> return (C.selectNextOperand repr cstk)
    V.EvKey V.KLeft [] -> return (C.selectPreviousOperand repr cstk)
    _ -> return cstk

renderBlockViewer :: forall arch s ir
                   . (C.Architecture arch s)
                  => C.AnalysisResult arch s
                  -> C.ContextStack arch s
                  -> BlockViewer arch s ir
                  -> B.Widget Names
renderBlockViewer _ares cs (BlockViewer names repr)
  | Just ctx <- cs ^? C.currentContext
  , Just blkState <- ctx ^. C.blockStateFor repr =
      let blk = blkState ^. C.blockStateBlock
          header = B.txt (Fmt.fmt ("Basic Block " +| C.prettyAddress (C.blockAddress blk) |+ ""))
          bl = mkBlockListState names blkState
          body = B.renderList (renderListItem (blkState ^. C.blockStateSelection)) False bl
      in B.borderWithLabel (B.hBox (map pad (irIndicators ++ [header]))) body
  | otherwise = B.borderWithLabel (B.txt "No block") (B.hBox (map pad irIndicators))
  where
    pad = B.padLeftRight 1
    irIndicators = map toIRLabel (C.SomeIRRepr C.BaseRepr : C.alternativeIRs (Proxy @(arch, s)))
    toIRLabel (C.SomeIRRepr r)
      | Just Refl <- testEquality r repr =
          B.withAttr B.listSelectedFocusedAttr (B.txt (Fmt.fmt ("[" +| showF r |+ "]")))
      | otherwise = B.txt (Fmt.fmt ("[" +| showF r |+ "]"))

-- | Construct a state for the block list widget on-demand based on the state in
-- the context
--
-- We could cache these in the context and update them constantly, but doing so
-- would add a brick dependency on the context, which is otherwise GUI-agnostic.
-- Constructing them here lets us move the context into core and reuse it
-- elsewhere.  Constructing them is also relatively cheap, as we can store the
-- underlying data vector, so we only need to construct the relatively simple
-- wrapper.
--
-- We do have to set the selection state, but that is cheap
mkBlockListState :: Names
                 -> C.BlockState arch s ir
                 -> B.GenericList Names V.Vector (Int, C.Address ir s, C.Instruction ir s)
mkBlockListState names blkState =
  l0 & B.listSelectedL .~ (C.selectedIndex (blkState ^. C.blockStateSelection))
  where
    l0 = B.list names (blkState ^. C.blockStateList) 1

renderListItem :: forall arch s
                . (C.IR arch s)
               => C.InstructionSelection arch s
               -> Bool
               -> (Int, C.Address arch s, C.Instruction arch s)
               -> B.Widget Names
renderListItem sel _isFocused (idx, addr, i) =
  B.hBox [ if C.showInstructionAddresses (Proxy @(arch, s))
           then B.padRight (B.Pad 2) (B.txt (C.prettyAddress addr))
           else B.emptyWidget
         , maybe B.emptyWidget (renderRawRepr i) C.rawRepr
         , renderListItemWithSelection sel idx addr i
         ]

renderRawRepr :: C.Instruction arch s
              -> (C.Instruction arch s -> Maybe BS.ByteString)
              -> B.Widget Names
renderRawRepr i asBytes =
  case asBytes i of
    Nothing -> B.txt "<error>"
    Just bs -> B.padRight (B.Pad 1) (B.hBox (map fmtByte (BS.unpack bs)))
  where
    fmtByte b = B.padRight (B.Pad 1) (B.str (printf "%02x" b))

renderListItemWithSelection :: (C.IR arch s)
                            => C.InstructionSelection arch s
                            -> Int
                            -> C.Address arch s
                            -> C.Instruction arch s
                            -> B.Widget Names
renderListItemWithSelection sel idx addr i =
  case sel of
    C.NoSelection -> renderInstruction addr i Nothing
    C.SingleSelection selIdx _addr Nothing
      | selIdx /= idx -> renderInstruction addr i Nothing
      | otherwise -> highlightWidget True (renderInstruction addr i Nothing)
    C.SingleSelection selIdx _addr (Just selectedOperand)
      | selIdx /= idx -> renderInstruction addr i Nothing
      | otherwise -> renderInstruction addr i (Just selectedOperand)
    C.MultipleSelection selIdx _addr tagged
      | idx == selIdx || idx `S.member` S.map fst tagged -> highlightWidget True (renderInstruction addr i Nothing)
      | otherwise -> renderInstruction addr i Nothing

highlightWidget :: Bool -> B.Widget n -> B.Widget n
highlightWidget isFocused w
  | isFocused = B.withAttr B.listSelectedFocusedAttr w
  | otherwise = w

-- | Render a single instruction without any highlighted operands
renderInstruction :: (C.IR arch s)
                  => C.Address arch s
                  -> C.Instruction arch s
                  -> Maybe Int
                  -> B.Widget Names
renderInstruction addr i mSelOperand =
  case C.boundValue i of
    Nothing -> B.hBox rhs
    Just bv -> B.hBox (B.txt (C.prettyOperand addr bv) : B.txt " = " : rhs)
  where
    opc = B.txt (C.prettyOpcode (C.opcode i))
    rhs = B.padRight (B.Pad 1) opc : L.intersperse (B.txt ",") operandWidgets
    tagOperand ctr op
      | C.operandSelectable op = (ctr + 1, (op, Just ctr))
      | otherwise = (ctr, (op, Nothing))
    (_, taggedOperands) = L.mapAccumL tagOperand 0 (C.operands i)
    operandWidgets = map renderOperand taggedOperands
    renderOperand (op, mIdx)
      | isJust mIdx && mSelOperand == mIdx = highlightWidget True (B.txt (C.prettyOperand addr op))
      | otherwise = B.txt (C.prettyOperand addr op)
