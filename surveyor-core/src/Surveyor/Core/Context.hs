{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
module Surveyor.Core.Context (
  Context(..),
  InstructionSelection(..),
  ContextStack(..),
  BlockState(..),
  emptyContextStack,
  makeContext,
  selectedIndex,
  -- * Modifiers
  resetBlockSelection,
  selectNextInstruction,
  selectPreviousInstruction,
  selectNextOperand,
  selectPreviousOperand,
  selectNextBlock,
  selectPreviousBlock,
  pushContext,
  -- *  Lenses
  currentContext,
  contextFocusedBlock,
  blockStateFor,
  blockStateList,
  blockStateBlock,
  blockStateSelection,
  vertexMapG,
  cfgG,
  selectedBlockL
  ) where

import           GHC.Generics ( Generic )

import           Control.DeepSeq ( NFData(rnf), deepseq )
import qualified Control.Lens as L
import           Control.Lens ( (&), (^.), (.~), (%~), (^?) )
import qualified Data.Foldable as F
import qualified Data.Generics.Product as GL
import qualified Data.Graph.Haggle as H
import qualified Data.Map as Map
import           Data.Maybe ( catMaybes, fromMaybe, listToMaybe )
import           Data.Parameterized.Classes ( atF )
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.TraversableF as TF
import           Data.Proxy ( Proxy(..) )
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Vector as V

import qualified Surveyor.Core.Architecture as CA
import qualified Surveyor.Core.IRRepr as IR
import qualified Surveyor.Core.TranslationCache as TC

-- | A context focused (at some point) by the user, and used to inform drawing
-- of various widgets.
--
-- A context is a pair of (Function, Block), as well as the state of any
-- selected instructions.  One UI goal to work towards is "bidirectional"
-- instruction selection.  Selecting a machine code instruction should select
-- the corresponding instruction(s) in macaw/crucible.  Selecting a
-- macaw/crucible instruction should select the machine instruction that
-- generated it.
--
-- Note that management of the context stack
-- allows for some policy choices.  A new context element could be created for
-- every movement of the instruction/operand focus if desired, allowing for
-- complete traceability.  Alternatively, instruction/operand selection state
-- could be "mutated" in place on the top-most record.
--
-- The more granular approaches would also allow commands like "pop to previous
-- function" or "pop to previous block", which are linear (but not too
-- expensive) operations.
data Context arch s =
  Context { cBaseBlock :: CA.Block arch s
          -- ^ The base focused basic block; this is duplicated from the
          -- BaseRepr entry in the map, but this lets us look up the base block
          -- in a total way (without worrying about the BaseRepr not being in
          -- the map).  The duplication isn't a problem because the 'CA.Block' is
          -- immutable.
          --
          -- We only need the block here, as each block also has a reference to
          -- its containing function.
          , cBlockStates :: MapF.MapF (IR.IRRepr arch) (BlockState arch s)
          -- ^ The set of instructions selected for each IR.  This should always
          -- be updated all at once to ensure consistency.
          , cSelectedBlock :: Maybe H.Vertex
          -- ^ For the function viewer, which block is selected.
          , cCFG :: H.PatriciaTree (CA.Block arch s) ()
          -- ^ The CFG of the function containing this block
          --
          -- FIXME: Come up with a nice way to re-use this when possible.
          --
          -- IDEA: Make pushContext only take a block, and re-use anything we
          -- can.  This will work, but requires a bit more API tweaking to let
          -- us compute something expensive (the new context) in a separate
          -- thread *and then* send a message when it is merged into the main state.
          , cVertexMap :: Map.Map (CA.Address arch s) H.Vertex
          }
  deriving (Generic)

vertexMapG :: L.Getter (Context arch s) (Map.Map (CA.Address arch s) H.Vertex)
vertexMapG = L.to cVertexMap

cfgG :: L.Getter (Context arch s) (H.PatriciaTree (CA.Block arch s) ())
cfgG = L.to cCFG

selectedBlockL :: L.Lens' (Context arch s) (Maybe H.Vertex)
selectedBlockL = GL.field @"cSelectedBlock"

instance (CA.ArchConstraints arch s) => NFData (Context arch s) where
  rnf c = cBaseBlock c `deepseq`
          TF.toListF forceBlockState (cBlockStates c) `deepseq` ()

forceBlockState :: BlockState arch s ir -> ()
forceBlockState bs@BlockState { withConstraints = withC } =
  withC (bsBlock bs `deepseq` bsSelection bs `deepseq` bsList bs `deepseq` ())

data BlockState arch s ir =
  BlockState { bsBlock :: CA.Block ir s
             , bsSelection :: InstructionSelection
             , bsList :: V.Vector (Int, CA.Address ir s, CA.Instruction ir s)
             -- ^ The list is the index of each instruction, the instruction
             -- address, and the instruction itself.
             --
             -- The index is there to make identifying selected instructions
             -- easier
             , withConstraints :: forall a . (CA.ArchConstraints ir s => a) -> a
             }

-- | Make a 'CA.Context' given a 'CA.FunctionHandle' and a 'CA.Block'
--
-- For each available alternative IR, sets up the appropriate 'CA.Block States'
makeContext :: forall arch s
             . (CA.Architecture arch s)
            => TC.TranslationCache arch s
            -> CA.AnalysisResult arch s
            -> CA.Block arch s
            -> IO (Context arch s)
makeContext tcache ares b = do
  let supportedIRs = CA.alternativeIRs (Proxy @(arch, s))
  blockStates <- catMaybes <$> mapM (\(CA.SomeIRRepr rep) -> makeBlockState tcache ares b rep) supportedIRs
  let baseState = BlockState { bsBlock = b
                             , bsSelection = NoSelection
                             , bsList = V.fromList [ (ix, addr, i)
                                                   | (ix, (addr, i)) <- zip [0..] (CA.blockInstructions b)
                                                   ]
                             , withConstraints = \a -> a
                             }
  let (cfg, vm) = mkCFG ares (CA.blockFunction b)
  return Context { cBaseBlock = b
                 , cBlockStates = MapF.fromList (MapF.Pair IR.BaseRepr baseState : blockStates)
                 , cCFG = cfg
                 , cVertexMap = vm
                 , cSelectedBlock = Just (minimum (Map.elems vm))
                 }

-- | Build a CFG for the function viewer
--
-- FIXME: The connectivity between blocks is currently not correct
mkCFG :: (CA.Architecture arch s)
      => CA.AnalysisResult arch s
      -> CA.FunctionHandle arch s
      -> (H.PatriciaTree (CA.Block arch s) (), Map.Map (CA.Address arch s) H.Vertex)
mkCFG ares fh = (gr, vm)
  where
    (_, gr, vm) = foldr addBlock (Nothing, H.emptyGraph, Map.empty) blocks
    blocks = CA.functionBlocks ares fh
    addBlock b (mpred, g, m) =
      let (v, g') = H.insertLabeledVertex g b
          vm' = Map.insert (CA.blockAddress b) v m
      in fromMaybe (Just v, g', vm') $ do
        p <- mpred
        (_, g'') <- H.insertLabeledEdge g' p v ()
        return (Just v, g'', vm')

makeBlockState :: forall arch ir s
                . (CA.Architecture arch s, CA.ArchConstraints ir s)
               => TC.TranslationCache arch s
               -> CA.AnalysisResult arch s
               -> CA.Block arch s
               -- ^ The original block that we want the alternative representation for
               -> IR.IRRepr arch ir
               -> IO (Maybe (MapF.Pair (IR.IRRepr arch) (BlockState arch s)))
makeBlockState tcache ares origBlock rep = do
  blockMapping <- TC.translateFunctionBlocks tcache ares rep fh
  case Map.lookup (CA.blockAddress origBlock) blockMapping of
    Nothing -> return Nothing
    Just (_, trBlock) -> do
      let insnList = [ (ix, addr, i)
                     | (ix, (addr, i)) <- zip [0..] (CA.blockInstructions trBlock)
                     ]
      let bs :: BlockState arch s ir
          bs = BlockState { bsSelection = NoSelection
                          , bsBlock = trBlock
                          , bsList = V.fromList insnList
                          , withConstraints = \a -> a
                          }
      return (Just (MapF.Pair rep bs))
  where
    fh = CA.blockFunction origBlock

contextBlockStates :: L.Lens' (Context arch s) (MapF.MapF (IR.IRRepr arch) (BlockState arch s))
contextBlockStates = GL.field @"cBlockStates"

contextFocusedBlock :: L.Getter (Context arch s) (CA.Block arch s)
contextFocusedBlock = L.to cBaseBlock

blockStateBlock :: L.Getter (BlockState arch s ir) (CA.Block ir s)
blockStateBlock = L.to bsBlock

blockStateList :: L.Getter (BlockState arch s ir) (V.Vector (Int, CA.Address ir s, CA.Instruction ir s))
blockStateList = L.to bsList

blockStateSelection :: L.Lens' (BlockState arch s ir) InstructionSelection
blockStateSelection f bs = fmap (\sel' -> bs { bsSelection = sel' }) (f (bsSelection bs))

data InstructionSelection = NoSelection
                          | SingleSelection Int (Maybe Int)
                          -- ^ A single instruction is selected and it may have an operand selected
                          | MultipleSelection Int (Set.Set Int)
                          -- ^ Multiple instructions are selected; operands may
                          -- not be selected in this case.  The primary
                          -- selection is the first Int.  The other selected
                          -- instructions are *tagged*
  deriving (Generic)

instance NFData InstructionSelection

selectedIndex :: InstructionSelection -> Maybe Int
selectedIndex i =
  case i of
    NoSelection -> Nothing
    SingleSelection ix _ -> Just ix
    MultipleSelection ix _ -> Just ix

data ContextStack arch s =
  ContextStack { cStack :: !(Seq.Seq (Context arch s))
               , cStackIndex :: Maybe Int
               }

emptyContextStack :: ContextStack arch s
emptyContextStack = ContextStack { cStack = Seq.empty
                                 , cStackIndex = Nothing
                                 }

-- | Push a new context onto the front of the context stack
--
-- If the pointer into the stack was pointing at something, the pointer is
-- modified to keep it at the same context frame.  Otherwise, the context frame
-- remains pointed at the top of the stack (the new entry)
pushContext :: Context arch s -> ContextStack arch s -> ContextStack arch s
pushContext c cs = ContextStack { cStack = c Seq.<| cStack cs
                                , cStackIndex = fmap (+1) (cStackIndex cs)
                                }

-- FIXME: export a read-only version of this.  We still need the traversal internally
currentContext :: L.Traversal' (ContextStack arch s) (Context arch s)
currentContext f cs
  | not (Seq.null (cStack cs)) =
    let sidx = fromMaybe 0 (cStackIndex cs)
        con x = cs { cStack = x }
    in con <$> (f (Seq.index (cStack cs) sidx) L.<&> \a -> Seq.update sidx a (cStack cs))
  | otherwise = pure cs

blockStateFor :: IR.IRRepr arch ir -> L.Lens' (Context arch s) (Maybe (BlockState arch s ir))
blockStateFor repr = contextBlockStates . atF repr

-- | Clear the selection of any individual instruction or operand from the current 'Context'
resetBlockSelection :: ContextStack arch s -> ContextStack arch s
resetBlockSelection cs =
  cs & currentContext . contextBlockStates %~ TF.fmapF clearSel
  where
    clearSel bs = bs & blockStateSelection .~ NoSelection

-- | Move the selection down one instruction (to the next instruction) in the current 'Context'
--
-- The 'IR.IRRepr' tells us which primary block state to update (while the rest
-- must be synced to that)
selectNextInstruction :: IR.IRRepr arch ir -> ContextStack arch s -> ContextStack arch s
selectNextInstruction = moveInstructionSelection (\nInsns n -> min (nInsns - 1) (n + 1))

selectPreviousInstruction :: IR.IRRepr arch ir -> ContextStack arch s -> ContextStack arch s
selectPreviousInstruction = moveInstructionSelection (\_nInsns n -> max 0 (n - 1))

-- | If possible, move the operand selection to the right by one
selectNextOperand :: (CA.IR ir s) => IR.IRRepr arch ir -> ContextStack arch s -> ContextStack arch s
selectNextOperand repr cs =
  cs & currentContext . contextBlockStates . atF repr %~ modifyOperandSelection xfrm (const 0)
  where
    xfrm nOperands n = min (nOperands - 1) (n + 1)

selectPreviousOperand :: (CA.IR ir s) => IR.IRRepr arch ir -> ContextStack arch s -> ContextStack arch s
selectPreviousOperand repr cs =
  cs & currentContext . contextBlockStates . atF repr %~ modifyOperandSelection xfrm (subtract 1)
  where
    xfrm _nOperands n = max 0 (n - 1)

modifyOperandSelection :: (CA.IR ir s)
                       => (Int -> Int -> Int)
                       -> (Int -> Int)
                       -> Maybe (BlockState arch s ir)
                       -> Maybe (BlockState arch s ir)
modifyOperandSelection xfrm def mbs = do
  bs <- mbs
  selIdx <- selectedIndex (bs ^. blockStateSelection)
  case (bs ^. blockStateList) V.!? selIdx of
    Nothing -> error ("Index out of bounds in modifyOperandSelection: " ++ show selIdx)
    Just (_, _, insn) -> return (bs & blockStateSelection %~ modifyOperand (length (CA.operands insn)))
  where
    modifyOperand nOperands i =
      case i of
        NoSelection -> NoSelection
        SingleSelection n Nothing -> SingleSelection n (Just (def nOperands))
        SingleSelection n (Just opIdx) -> SingleSelection n (Just (xfrm nOperands opIdx))
        MultipleSelection {} -> i

moveBlockSelection :: (H.PatriciaTree (CA.Block arch s) () -> H.Vertex -> [H.Vertex])
                   -> ContextStack arch s
                   -> ContextStack arch s
moveBlockSelection succOrPred cs0
  | Just ctx <- cs0 ^? currentContext =
      case ctx ^. selectedBlockL of
        Nothing -> cs0 & currentContext . selectedBlockL .~ Just (minimum (Map.elems (ctx ^. vertexMapG)))
        Just v -> cs0 & currentContext . selectedBlockL .~ listToMaybe (succOrPred (ctx ^. cfgG) v)
  | otherwise = cs0

selectNextBlock :: ContextStack arch s -> ContextStack arch s
selectNextBlock = moveBlockSelection H.successors

selectPreviousBlock :: ContextStack arch s -> ContextStack arch s
selectPreviousBlock = moveBlockSelection H.predecessors

-- | The underlying logic for selecting the next or previous instruction
--
-- This is complicated because state has to be synced across our internal view
-- and the Brick list UI widget view.  Moreover, moving the selection in one
-- view forces us to sync across the other views, too.
moveInstructionSelection :: (Int -> Int -> Int)
                         -- ^ A numeric transformation on the currently-selected index
                         -> IR.IRRepr arch ir
                         -> ContextStack arch s
                         -> ContextStack arch s
moveInstructionSelection modSel repr cs0 =
  cs1 & currentContext . contextBlockStates %~ TF.fmapF (syncOtherStates repr)
  where
    cs1 = cs0 & currentContext . contextBlockStates . atF repr %~ moveNext
    moveNext Nothing = Nothing
    moveNext (Just bs) = Just (bs & blockStateSelection %~ modifySelection modSel bs)

-- | Given a 'BlockState', sync all of the selected instructions of the
-- different IRs to the state of the IR with the given repr.
--
-- This requires support from the underlying class infrastructure to determine
-- the correspondences between instructions.
--
-- FIXME: Implement this
syncOtherStates :: IR.IRRepr arch targetIR -> BlockState arch s ir -> BlockState arch s ir
syncOtherStates _ = id

-- | Increment the current selection
--
-- If nothing is selected, we select the first instruction
modifySelection :: (Int -> Int -> Int) -> BlockState arch s ir -> InstructionSelection -> InstructionSelection
modifySelection modSel bs i =
  case i of
    NoSelection
      | nInsns > 0 -> SingleSelection 0 Nothing
      | otherwise -> NoSelection
    SingleSelection n _ -> SingleSelection (modSel nInsns n) Nothing
    MultipleSelection n sels -> MultipleSelection (modSel nInsns n) sels
  where
    nInsns = F.length (bs ^. blockStateList)
