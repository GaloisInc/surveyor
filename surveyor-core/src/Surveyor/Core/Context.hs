{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
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
  contextForward,
  contextBack,
  -- *  Lenses
  currentContext,
  symExecSessionIDL,
  baseFunctionG,
  blockStateFor,
  blockStateList,
  blockStateBlock,
  blockStateSelection,
  functionStateFor,
  vertexMapG,
  cfgG,
  selectedBlockL
  ) where

import           GHC.Generics ( Generic )

import           Control.DeepSeq ( NFData(rnf), deepseq )
import           Control.Lens ( (&), (^.), (.~), (%~), (^?) )
import qualified Control.Lens as L
import           Control.Monad ( guard )
import qualified Data.Foldable as F
import qualified Data.Graph.Haggle as H
import qualified Data.Map as Map
import           Data.Maybe ( catMaybes, fromMaybe, listToMaybe, mapMaybe, maybeToList )
import           Data.Parameterized.Classes ( testEquality, (:~:)(Refl), atF )
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.Nonce as PN
import qualified Data.Parameterized.TraversableF as TF
import           Data.Proxy ( Proxy(..) )
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Vector as V

import qualified Surveyor.Core.Architecture as CA
import qualified Surveyor.Core.IRRepr as IR
import qualified Surveyor.Core.TranslationCache as TC
import qualified Surveyor.Core.SymbolicExecution as SE
import qualified Surveyor.Core.OperandList as OL
import qualified Surveyor.Core.Panic as SCP

data InstructionSelection ir s = NoSelection
                               | SingleSelection Int (CA.Address ir s) (Maybe (OL.Zipper (Int, CA.Operand ir s)))
                               -- ^ A single instruction is selected and it may have an operand selected
                               | MultipleSelection Int (CA.Address ir s) (Set.Set (Int, CA.Address ir s))
                               -- ^ Multiple instructions are selected; operands may
                               -- not be selected in this case.  The primary
                               -- selection is the first Int.  The other selected
                               -- instructions are *tagged*
                               | TransientSelection Int (CA.Address ir s) (Set.Set (Int, CA.Address ir s))
                               -- ^ A transient selection that was created by an
                               -- automatic action, but that goes away
                               -- (degenerates to a single selection) as soon as
                               -- the user moves the cursor
  deriving (Generic)

instance (NFData (CA.Address ir s), NFData (CA.Operand ir s)) => NFData (InstructionSelection ir s)

data BlockState arch s ir =
  BlockState { bsBlock :: CA.Block ir s
             , bsSelection :: InstructionSelection ir s
             , bsList :: V.Vector (Int, CA.Address ir s, CA.Instruction ir s)
             -- ^ The list is the index of each instruction, the instruction
             -- address, and the instruction itself.
             --
             -- The index is there to make identifying selected instructions
             -- easier
             , bsBlockMapping :: Maybe (CA.BlockMapping arch ir s)
             -- ^ We keep the whole block mapping here so that we can sync up
             -- block selection states across IRs.  We won't necessarily have a
             -- block map for two reasons:
             --
             -- 1) The Base IR won't have one
             --
             -- 2) Translation could fail for a block
             , bsWithConstraints :: forall a . (CA.ArchConstraints ir s => a) -> a
             -- ^ A way to recover the 'CA.ArchConstraints' dictionary
             , bsRepr :: IR.IRRepr arch ir
             }

data FunctionState arch s ir =
  FunctionState { fsSelectedBlock :: Maybe H.Vertex
                -- ^ The currently-selected block for this IR, if any
                , fsCFG :: H.PatriciaTree (CA.Block ir s) ()
                -- ^ The CFG of the current function
                --
                -- FIXME: Try to cache these somewhere
                , fsVertexMap :: Map.Map (CA.Address ir s) H.Vertex
                -- ^ A mapping from block addresses to vertices
                , fsBaseFunction :: CA.FunctionHandle arch s
                -- ^ The base function that this block corresponds to
                , fsWithConstraints :: forall a . (CA.ArchConstraints ir s => a) -> a
                }

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
  Context { cBlockState :: MapF.MapF (IR.IRRepr arch) (BlockState arch s)
          -- ^ The set of instructions selected for each IR.  This should always
          -- be updated all at once to ensure consistency.
          , cFunctionState :: MapF.MapF (IR.IRRepr arch) (FunctionState arch s)
          -- ^ State information for the currently-selected function (in each IR)
          , cSymExecSessionID :: SE.SessionID s
          -- ^ State required for symbolic execution
          --
          -- We always have at least a default configuration. We lazily
          -- instantiate the rest as-needed
          , cBaseFunction :: CA.FunctionHandle arch s
          -- ^ The function handle for the function focused by this context
          }
  deriving (Generic)

L.makeLensesFor
  [ ("cSymExecSessionID", "symExecSessionIDL")
  , ("cFunctionState", "functionStateL")
  , ("cBlockState", "blockStateL") ]
  ''Context

vertexMapG :: L.Getter (FunctionState arch s ir) (Map.Map (CA.Address ir s) H.Vertex)
vertexMapG = L.to fsVertexMap

cfgG :: L.Getter (FunctionState arch s ir) (H.PatriciaTree (CA.Block ir s) ())
cfgG = L.to fsCFG

selectedBlockL :: L.Lens' (FunctionState arch s ir) (Maybe H.Vertex)
selectedBlockL f fs = fmap (\sb' -> fs { fsSelectedBlock = sb' }) (f (fsSelectedBlock fs))

instance (CA.ArchConstraints arch s) => NFData (Context arch s) where
  rnf c = TF.toListF forceFunctionState (cFunctionState c) `deepseq` TF.toListF forceBlockState (cBlockState c) `deepseq` ()

forceBlockState :: BlockState arch s ir -> ()
forceBlockState bs@BlockState { bsWithConstraints = withC } =
  withC (bsBlock bs `deepseq` bsSelection bs `deepseq` bsList bs `deepseq` ())

forceFunctionState :: FunctionState arch s ir -> ()
forceFunctionState fs@(FunctionState { fsWithConstraints = withC }) =
  withC (fsSelectedBlock fs `deepseq` fsCFG fs `deepseq` fsVertexMap fs `deepseq` ())

-- | Make a 'CA.Context' given a 'CA.FunctionHandle' and a 'CA.Block'
--
-- For each available alternative IR, sets up the appropriate 'CA.Block States'
--
-- If the requested IR is not the base, don't populate any of the translation
-- stuff (or extra block IRs)
makeContext :: forall arch s ir
             . (CA.Architecture arch s, CA.ArchConstraints ir s)
            => PN.NonceGenerator IO s
            -> TC.TranslationCache arch s
            -> CA.AnalysisResult arch s
            -> CA.FunctionHandle arch s
            -> IR.IRRepr arch ir
            -> CA.Block ir s
            -> IO (Context arch s, SE.SymbolicExecutionState arch s SE.Config)
makeContext ng tcache ares fh irrepr b =
  case irrepr of
    IR.BaseRepr -> do
      let supportedIRs = CA.alternativeIRs (Proxy @(arch, s))
      let makeIRBlock (CA.SomeIRRepr rep) = do
            mbs <- makeBlockState tcache ares fh b rep
            case mbs of
              Nothing -> return Nothing
              Just bs -> return (Just (MapF.Pair rep bs))
      blockStates <- catMaybes <$> mapM makeIRBlock supportedIRs
      let baseState = BlockState { bsBlock = b
                                 , bsSelection = NoSelection
                                 , bsList = toInstructionList b
                                 , bsBlockMapping = Nothing
                                 , bsWithConstraints = \a -> a
                                 , bsRepr = IR.BaseRepr
                                 }
      let (baseCFG, baseVertexMap) = mkCFG (CA.functionBlocks ares fh)
      let baseFunctionState = FunctionState { fsSelectedBlock = Nothing
                                            , fsCFG = baseCFG
                                            , fsVertexMap = baseVertexMap
                                            , fsBaseFunction = fh
                                            , fsWithConstraints = \a -> a
                                            }
      let makeIRFunction (CA.SomeIRRepr rep) = fmap (MapF.Pair rep) <$> makeFunctionState tcache ares fh rep
      funcStates <- catMaybes <$> mapM makeIRFunction supportedIRs

      ses0 <- SE.initialSymbolicExecutionState ng
      let ctx = Context { cBlockState = MapF.fromList (MapF.Pair IR.BaseRepr baseState : blockStates)
                        , cFunctionState = MapF.fromList (MapF.Pair IR.BaseRepr baseFunctionState : funcStates)
                        , cBaseFunction = fh
                        , cSymExecSessionID = SE.symbolicSessionID ses0
                        }
      return (ctx, ses0)
    _ -> do
      ses0 <- SE.initialSymbolicExecutionState ng
      bs <- makeAlternativeBlockState b irrepr
      mfs <- fmap (MapF.Pair irrepr) <$> makeFunctionState tcache ares fh irrepr
      let ctx = Context { cBlockState = MapF.fromList [ MapF.Pair irrepr bs ]
                        , cFunctionState = MapF.fromList (maybeToList mfs)
                        , cBaseFunction = fh
                        , cSymExecSessionID = SE.symbolicSessionID ses0
                        }
      return (ctx, ses0)

makeFunctionState :: (CA.Architecture arch s, CA.ArchConstraints ir s)
                  => TC.TranslationCache arch s
                  -> CA.AnalysisResult arch s
                  -> CA.FunctionHandle arch s
                  -- ^ The handle to the base IR function to construct a state for
                  -> IR.IRRepr arch ir
                  -- ^ The IR to construct a state for
                  -> IO (Maybe (FunctionState arch s ir))
makeFunctionState tcache ares fh repr = do
  mblks <- TC.translateFunctionBlocks tcache ares repr fh
  case mblks of
    Nothing -> return Nothing
    Just (blks, _blkMapping) -> do
      let (g, addrMap) = mkCFG blks
      let fs = FunctionState { fsSelectedBlock = Nothing
                             , fsCFG = g
                             , fsVertexMap = addrMap
                             , fsBaseFunction = fh
                             , fsWithConstraints = \a -> a
                             }
      return (Just fs)

-- | Build a CFG for the function viewer
--
-- FIXME: The connectivity between blocks is currently not correct.  To fix it,
-- we'll need an abstraction for computing connectivity (successors) at the
-- Architcture.Class.Block level.
mkCFG :: (Ord (CA.Address ir s))
      => [CA.Block ir s]
      -> (H.PatriciaTree (CA.Block ir s) (), Map.Map (CA.Address ir s) H.Vertex)
mkCFG blocks = (gr, vm)
  where
    (_, gr, vm) = foldr addBlock (Nothing, H.emptyGraph, Map.empty) blocks
    addBlock b (mpred, g, m) =
      let (v, g') = H.insertLabeledVertex g b
          vm' = Map.insert (CA.blockAddress b) v m
      in fromMaybe (Just v, g', vm') $ do
        p <- mpred
        (_, g'') <- H.insertLabeledEdge g' p v ()
        return (Just v, g'', vm')

toInstructionList :: CA.Block ir s
                  -> V.Vector (Int, CA.Address ir s, CA.Instruction ir s)
toInstructionList block =
  V.fromList indexedAddrs
  where
    indexedAddrs = [ (ix, addr, i)
                   | (ix, (addr, i)) <- zip [0..] (CA.blockInstructions block)
                   ]

makeAlternativeBlockState :: (Monad m, CA.ArchConstraints ir s)
                          => CA.Block ir s
                          -> IR.IRRepr arch ir
                          -> m (BlockState arch s ir)
makeAlternativeBlockState block rep =
  return BlockState { bsSelection = NoSelection
                    , bsBlock = block
                    , bsList = toInstructionList block
                    , bsRepr = rep
                    , bsWithConstraints = \a -> a
                    , bsBlockMapping = Nothing
                    }

makeBlockState :: forall arch ir s
                . (CA.Architecture arch s, CA.ArchConstraints ir s)
               => TC.TranslationCache arch s
               -> CA.AnalysisResult arch s
               -> CA.FunctionHandle arch s
               -> CA.Block arch s
               -- ^ The original block that we want the alternative representation for
               -> IR.IRRepr arch ir
               -> IO (Maybe (BlockState arch s ir))
makeBlockState tcache ares fh origBlock rep = do
  mBlockMapping <- TC.translateFunctionBlocks tcache ares rep fh
  return $ do
    bm <- mBlockMapping
    (_, trBlock) <- Map.lookup (CA.blockAddress origBlock) (CA.blockMapping (snd bm))
    let bs :: BlockState arch s ir
        bs = BlockState { bsSelection = NoSelection
                        , bsBlock = trBlock
                        , bsList = toInstructionList trBlock
                        , bsBlockMapping = Just (snd bm)
                        , bsWithConstraints = \a -> a
                        , bsRepr = rep
                        }
    return bs

baseFunctionG :: L.Getter (Context arch s) (CA.FunctionHandle arch s)
baseFunctionG = L.to cBaseFunction

blockStateBlock :: L.Getter (BlockState arch s ir) (CA.Block ir s)
blockStateBlock = L.to bsBlock

blockStateList :: L.Getter (BlockState arch s ir) (V.Vector (Int, CA.Address ir s, CA.Instruction ir s))
blockStateList = L.to bsList

blockStateSelection :: L.Lens' (BlockState arch s ir) (InstructionSelection ir s)
blockStateSelection f bs = fmap (\sel' -> bs { bsSelection = sel' }) (f (bsSelection bs))

selectedIndex :: InstructionSelection ir s -> Maybe Int
selectedIndex i =
  case i of
    NoSelection -> Nothing
    SingleSelection ix _ _ -> Just ix
    MultipleSelection ix _ _ -> Just ix
    TransientSelection ix _ _ -> Just ix

data ContextStack arch s =
  ContextStack { cStack :: !(Seq.Seq (Context arch s))
               , cStackIndex :: Maybe Int
               }

emptyContextStack :: ContextStack arch s
emptyContextStack = ContextStack { cStack = Seq.empty
                                 , cStackIndex = Nothing
                                 }

contextBack :: ContextStack arch s -> ContextStack arch s
contextBack cs = cs { cStackIndex = nextIndex }
  where
    nextIndex = fromMaybe (cStackIndex cs) $ do
      let ix = maybe 1 (+1) (cStackIndex cs)
      guard (ix < Seq.length (cStack cs))
      return (Just ix)

contextForward :: ContextStack arch s -> ContextStack arch s
contextForward cs = cs { cStackIndex = nextIndex }
  where
    nextIndex = do
      curIdx <- cStackIndex cs
      let next = curIdx - 1
      guard (next > 0)
      return next

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
blockStateFor repr = blockStateL . atF repr

functionStateFor :: IR.IRRepr arch ir -> L.Lens' (Context arch s) (Maybe (FunctionState arch s ir))
functionStateFor repr = functionStateL . atF repr

-- | Clear the selection of any individual instruction or operand from the current 'Context'
resetBlockSelection :: ContextStack arch s -> ContextStack arch s
resetBlockSelection cs =
  cs & currentContext . blockStateL %~ TF.fmapF clearSel
  where
    clearSel bs = bs & blockStateSelection .~ NoSelection

-- | Move the selection down one instruction (to the next instruction) in the current 'Context'
--
-- The 'IR.IRRepr' tells us which primary block state to update (while the rest
-- must be synced to that)
selectNextInstruction :: (Ord (CA.Address ir s), Ord (CA.Address arch s), Show (CA.Address arch s))
                      => IR.IRRepr arch ir
                      -> ContextStack arch s
                      -> ContextStack arch s
selectNextInstruction = moveInstructionSelection (\nInsns n -> min (nInsns - 1) (n + 1))

selectPreviousInstruction :: (Ord (CA.Address ir s), Ord (CA.Address arch s), Show (CA.Address arch s))
                          => IR.IRRepr arch ir
                          -> ContextStack arch s
                          -> ContextStack arch s
selectPreviousInstruction = moveInstructionSelection (\_nInsns n -> max 0 (n - 1))

-- | If possible, move the operand selection to the right by one
selectNextOperand :: (CA.IR ir s) => IR.IRRepr arch ir -> ContextStack arch s -> ContextStack arch s
selectNextOperand repr cs =
  cs & currentContext . blockStateL . atF repr %~ modifyOperandSelection OL.zipperNext

selectPreviousOperand :: (CA.IR ir s) => IR.IRRepr arch ir -> ContextStack arch s -> ContextStack arch s
selectPreviousOperand repr cs =
  cs & currentContext . blockStateL . atF repr %~ modifyOperandSelection OL.zipperPrev

modifyOperandSelection :: (CA.IR ir s)
                       => (forall a . OL.Zipper a -> Maybe (OL.Zipper a))
                       -> Maybe (BlockState arch s ir)
                       -> Maybe (BlockState arch s ir)
modifyOperandSelection modz mbs = do
  bs <- mbs
  selIdx <- selectedIndex (bs ^. blockStateSelection)
  case (bs ^. blockStateList) V.!? selIdx of
    Nothing -> SCP.panic "modifyOperandSelection" [ "Index out of bounds in modifyOperandSelection: " ++ show selIdx
                                                  ]
    Just (_, _, insn) -> return (bs & blockStateSelection %~ modifyOperand insn)
  where
    modifyOperand insn i =
      case i of
        NoSelection -> NoSelection
        SingleSelection n addr Nothing ->
          let mz0 = OL.zipper (OL.indexOperandList (CA.operands insn))
          in case mz0 of
            Just z0
              | CA.operandSelectable (snd (OL.zipperFocused z0)) -> SingleSelection n addr (Just z0)
              | otherwise -> SingleSelection n addr (applyUntil (CA.operandSelectable . snd) modz z0)
            Nothing -> SingleSelection n addr Nothing
        SingleSelection n addr (Just z0) ->
          SingleSelection n addr (applyUntil (CA.operandSelectable . snd) modz z0)
        MultipleSelection {} -> i
        TransientSelection {} -> i

applyUntil :: (a -> Bool)
           -> (OL.Zipper a -> Maybe (OL.Zipper a))
           -> OL.Zipper a
           -> Maybe (OL.Zipper a)
applyUntil p op z = fromMaybe (Just z) $ do
  nz <- op z
  let elt = OL.zipperFocused nz
  if | p elt -> return (Just nz)
     | otherwise -> return (applyUntil p op nz)


moveBlockSelection :: (H.PatriciaTree (CA.Block ir s) () -> H.Vertex -> [H.Vertex])
                   -> IR.IRRepr arch ir
                   -> ContextStack arch s
                   -> ContextStack arch s
moveBlockSelection succOrPred irep cs0
  | Just ctx <- cs0 ^? currentContext
  , Just fstate <- MapF.lookup irep (ctx ^. functionStateL) =
      case fstate ^. selectedBlockL of
        Nothing ->
          let nextSel = Just (minimum (Map.elems (fstate ^. vertexMapG)))
          in cs0 & currentContext . functionStateL %~ MapF.insert irep (fstate & selectedBlockL .~ nextSel)
        Just v ->
          let nextSel = listToMaybe (succOrPred (fstate ^. cfgG) v)
          in cs0 & currentContext . functionStateL %~ MapF.insert irep (fstate & selectedBlockL .~ nextSel)
  | otherwise = cs0

selectNextBlock :: IR.IRRepr arch ir -> ContextStack arch s -> ContextStack arch s
selectNextBlock = moveBlockSelection H.successors

selectPreviousBlock :: IR.IRRepr arch ir -> ContextStack arch s -> ContextStack arch s
selectPreviousBlock = moveBlockSelection H.predecessors

-- | The underlying logic for selecting the next or previous instruction
--
-- This is complicated because state has to be synced across our internal view
-- and the Brick list UI widget view.  Moreover, moving the selection in one
-- view forces us to sync across the other views, too.
moveInstructionSelection :: (Ord (CA.Address ir s), Ord (CA.Address arch s), Show (CA.Address arch s))
                         => (Int -> Int -> Int)
                         -- ^ A numeric transformation on the currently-selected index
                         -> IR.IRRepr arch ir
                         -> ContextStack arch s
                         -> ContextStack arch s
moveInstructionSelection modSel repr cs0 =
  cs2 & currentContext . blockStateL %~ TF.fmapF (syncOtherStates repr (cs2 ^? currentContext . blockStateL))
  where
    cs1 = cs0 & currentContext . blockStateL . atF repr %~ moveNext
    cs2 = cs1 & currentContext . blockStateL . atF IR.BaseRepr %~ syncBaseState repr (cs1 ^? currentContext . blockStateL)
    moveNext Nothing = Nothing
    moveNext (Just bs) = Just (bs & blockStateSelection %~ modifySelection modSel bs)

-- | Sync the base IR state to the target IR state using irToBaseAddrs (unless base is target)
syncBaseState :: (Ord (CA.Address targetIR s), Ord (CA.Address arch s))
              => IR.IRRepr arch targetIR
              -- ^ The IR whose state that the user updated
              -> Maybe (MapF.MapF (IR.IRRepr arch) (BlockState arch s))
              -- ^ The current states (where the target IR has been updated, but
              -- none of the others have).  It will always be a Just, but our
              -- lens doesn't really let us state that easily.
              -> Maybe (BlockState arch s arch)
              -- ^ State to update
              -> Maybe (BlockState arch s arch)
syncBaseState _ _ Nothing = Nothing
syncBaseState updatedIR mBlkStMap (Just bs)
  | Just Refl <- testEquality updatedIR (bsRepr bs) = Just bs
  | otherwise = fromMaybe (Just bs) $ do
      blkStMap <- mBlkStMap
      updatedBS <- MapF.lookup updatedIR blkStMap
      bm <- bsBlockMapping updatedBS
      case bsSelection updatedBS of
        NoSelection -> return (Just bs)
        SingleSelection _ix addr _ -> do
          baseAddrs <- Map.lookup addr (CA.irToBaseAddrs bm)
          -- Find the indices of the addrs here in bs
          makeNextSelection baseAddrs
        MultipleSelection _ix addr tagged -> do
          let addrs = addr : map snd (F.toList tagged)
          let baseAddrs = Set.unions (mapMaybe (\a -> Map.lookup a (CA.irToBaseAddrs bm)) addrs)
          makeNextSelection baseAddrs
        TransientSelection _ix addr tagged -> do
          let addrs = addr : map snd (F.toList tagged)
          let baseAddrs = Set.unions (mapMaybe (\a -> Map.lookup a (CA.irToBaseAddrs bm)) addrs)
          makeNextSelection baseAddrs
  where
    makeNextSelection baseAddrs =
      case F.foldr (addSelectedAddrs baseAddrs) [] (bsList bs) of
        [] -> return (Just bs)
        [(singIx, singAddr)] ->
          return $ Just bs { bsSelection = TransientSelection singIx singAddr Set.empty }
        (selIx, selAddr) : rest ->
          return $ Just bs { bsSelection = TransientSelection selIx selAddr (Set.fromList rest) }

addSelectedAddrs :: (Ord (CA.Address ir s))
                 => Set.Set (CA.Address ir s)
                 -> (Int, CA.Address ir s, a)
                 -> [(Int, CA.Address ir s)]
                 -> [(Int, CA.Address ir s)]
addSelectedAddrs selAddrs (ix, addr, _i) lst =
  case Set.member addr selAddrs of
    True -> (ix, addr) : lst
    False -> lst

-- | Given a 'BlockState', sync all of the selected instructions of the
-- different IRs to the state of the IR with the given repr.
--
-- This requires support from the underlying class infrastructure to determine
-- the correspondences between instructions.
--
-- The idea is that we'll first update the base IR (outside of this function)
-- and then, for each IR:
--
-- * If the IR is the modified IR (passed as an argument), do nothing (already updated)
--
-- * If the IR is base, do nothing (already updated)
--
-- * Otherwise, use the baseToIRAddrs map to update this state
syncOtherStates :: (Ord (CA.Address arch s), Show (CA.Address arch s))
                => IR.IRRepr arch targetIR
                -> Maybe (MapF.MapF (IR.IRRepr arch) (BlockState arch s))
                -> BlockState arch s ir
                -> BlockState arch s ir
syncOtherStates updatedIR mBlkStMap bs
  | Just Refl <- testEquality updatedIR (bsRepr bs) = bs
  | Just Refl <- testEquality IR.BaseRepr (bsRepr bs) = bs
  | otherwise = bsWithConstraints bs $ fromMaybe bs $ do
      blkStMap <- mBlkStMap
      -- The Base IR has been synced to whatever the update was, so we can use
      -- 'baseToIRAddrs' (of *bs*) to figure out what should be selected.
      baseBS <- MapF.lookup IR.BaseRepr blkStMap
      bm <- bsBlockMapping bs
      case bsSelection baseBS of
        NoSelection -> return bs
        SingleSelection _ix addr _ -> do
          irAddrs <- Map.lookup addr (CA.baseToIRAddrs bm)
          makeNextSelection irAddrs
        MultipleSelection _ix addr tagged -> do
          let oldIRSels = addr : map snd (F.toList tagged)
          let irAddrs = Set.unions (mapMaybe (\a -> Map.lookup a (CA.baseToIRAddrs bm)) oldIRSels)
          makeNextSelection irAddrs
        TransientSelection _ix addr tagged -> do
          let oldIRSels = addr : map snd (F.toList tagged)
          let irAddrs = Set.unions (mapMaybe (\a -> Map.lookup a (CA.baseToIRAddrs bm)) oldIRSels)
          makeNextSelection irAddrs
  where
    makeNextSelection irAddrs = bsWithConstraints bs $
      case F.foldr (addSelectedAddrs irAddrs) [] (bsList bs) of
        [] -> return bs
        [(singIx, singAddr)] ->
          return bs { bsSelection = TransientSelection singIx singAddr Set.empty }
        (selIx, selAddr) : rest ->
          return bs { bsSelection = TransientSelection selIx selAddr (Set.fromList rest) }


-- | Increment the current selection
--
-- If nothing is selected, we select the first instruction
modifySelection :: (Int -> Int -> Int) -> BlockState arch s ir -> InstructionSelection ir s -> InstructionSelection ir s
modifySelection modSel bs i =
  case i of
    NoSelection
      | nInsns > 0 ->
        let (_, addr, _) = bsList bs V.! 0
        in SingleSelection 0 addr Nothing
      | otherwise -> NoSelection
    SingleSelection n _ _ -> fromMaybe i $ do
      let newIdx = modSel nInsns n
      (_, addr, _) <- bsList bs V.!? newIdx
      return (SingleSelection newIdx addr Nothing)
    MultipleSelection n _ sels -> fromMaybe i $ do
      let newIdx = modSel nInsns n
      (_, addr, _) <- bsList bs V.!? newIdx
      return (MultipleSelection newIdx addr sels)
    TransientSelection n _ _ -> fromMaybe i $ do
      let newIdx = modSel nInsns n
      (_, addr, _) <- bsList bs V.!? newIdx
      return (SingleSelection newIdx addr Nothing)
  where
    nInsns = F.length (bs ^. blockStateList)
