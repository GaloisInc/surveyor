{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module Surveyor.Core.BinaryAnalysisResult (
  BinaryAnalysisResult(..),
  AddressIndex,
  indexAddresses,
  largestFunction,
  blocksContaining
  ) where

import           Control.Lens ( (^.) )
import qualified Data.Foldable as F
import qualified Data.IntervalMap as IM
import qualified Data.List as L
import qualified Data.Macaw.BinaryLoader as MBL
import qualified Data.Macaw.CFG as MC
import qualified Data.Macaw.Discovery.State as MD
import qualified Data.Macaw.Memory as MM
import           Data.Maybe ( mapMaybe )
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.Nonce as NG
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Map as Map
import qualified Data.Set.NonEmpty as NES
import qualified Lang.Crucible.Backend.Simple as SB
import qualified Renovate as R
import qualified SemMC.Formula as F

data BinaryAnalysisResult s o arch =
  forall binFmt fs .
  BinaryAnalysisResult { rBlockInfo :: !(R.BlockInfo arch)
                       , rNonceGen :: NG.NonceGenerator IO s
                       , rLoadedBinary :: !(MBL.LoadedBinary arch binFmt)
                       , rISA :: R.ISA arch
                       , rAddressIndex :: !(AddressIndex arch)
                       , rNonce :: NG.Nonce s arch
                       , rSemantics :: Maybe (MapF.MapF o (F.ParameterizedFormula (SB.SimpleBackend s fs) arch))
                       , rSym :: SB.SimpleBackend s fs
                       }

data AddressIndex arch =
  AddressIndex { aiBlockRanges :: !(IM.IntervalMap (MM.MemAddr (MC.ArchAddrWidth arch)) (R.ConcreteBlock arch, NES.Set (R.ConcreteAddress arch)))
               -- ^ The address ranges spanned by each block in the program
               --
               -- The payload is the block and the set of functions that contain it
               , aiFunctionMap :: !(Map.Map (R.ConcreteAddress arch) (Some (MD.DiscoveryFunInfo arch)))
               }

indexAddresses :: (MM.MemWidth (MC.ArchAddrWidth arch))
               => R.ISA arch
               -> R.BlockInfo arch
               -> AddressIndex arch
indexAddresses isa bi =
  AddressIndex { aiBlockRanges = blRngIdx
               , aiFunctionMap = funcMap
               }
  where
    (blRngIdx, funcMap) = F.foldl' indexFunction (IM.empty, Map.empty) (Map.toList (R.biFunctions bi))
    indexFunction (brng, fm) (faddr, (blocks, dfi)) =
      let brng' = F.foldl' (indexBlock faddr) brng blocks
          fm' = Map.insert faddr dfi fm
      in (brng', fm')
    indexBlock faddr brng block =
      -- Note: blocks can be shared by multiple functions, so we have to be
      -- prepared to do a merge if we encounter a block more than once.
      let iaddrs = fmap (MM.absoluteAddr . R.absoluteAddress . snd) (R.instructionAddresses isa block)
          mergeFunctionSets (b, s1) (_, s2) = (b, s1 <> s2)
      in IM.insertWith mergeFunctionSets (IM.ClosedInterval (minimum iaddrs) (maximum iaddrs)) (block, NES.singleton faddr) brng

-- | Look up the basic block containing the given address.
--
-- The 'Word64' is converted to a 'MM.MemAddr'.
blocksContaining :: BinaryAnalysisResult s o arch -> MM.MemAddr (MC.ArchAddrWidth arch) -> [(R.ConcreteBlock arch, NES.Set (R.ConcreteAddress  arch))]
blocksContaining bar addr = IM.elems (IM.containing (aiBlockRanges (rAddressIndex bar)) addr)

-- | From a set of functions, pick the largest (based on the sum of block sizes)
largestFunction :: BinaryAnalysisResult s o arch -> NES.Set (R.ConcreteAddress arch) -> (R.ConcreteAddress arch, Some (MD.DiscoveryFunInfo arch))
largestFunction bar nes =
  L.maximumBy funcSize funcs
  where
    aix = rAddressIndex bar
    toFunction addr = (addr,) <$> Map.lookup addr (aiFunctionMap aix)
    -- The mapMaybe shouldn't lose any information, as the function index is
    -- built at the same time as the block index with the same set of functions.
    -- It would be nice to statically prove that.
    funcs = mapMaybe toFunction (F.toList (NES.flatten nes))
    funcSize (_, Some dfi1) (_, Some dfi2) = compare (fsz dfi1) (fsz dfi2)
    fsz dfi = sum (map MD.blockSize (Map.elems (dfi ^. MD.parsedBlocks)))
