{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module Surveyor.BinaryAnalysisResult (
  BinaryAnalysisResult(..),
  indexBlocksByAddress,
  blocksContaining
  ) where

import qualified Data.Foldable as F
import qualified Data.IntervalMap as IM
import qualified Data.Macaw.Memory as MM
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.Nonce as NG
import qualified Lang.Crucible.Solver.SimpleBackend as SB
import qualified Renovate as R
import qualified SemMC.Formula as F

data BinaryAnalysisResult s o i a w arch =
  BinaryAnalysisResult { rBlockInfo :: !(R.BlockInfo i w arch)
                       , rMemory :: !(MM.Memory w)
                       , rISA :: R.ISA i a w
                       , rBlockMap :: !(IM.IntervalMap (MM.MemAddr w) (R.ConcreteBlock i w))
                       , rNonce :: NG.Nonce s arch
                       , rSemantics :: Maybe (MapF.MapF o (F.ParameterizedFormula (SB.SimpleBackend s) arch))
                       }

indexBlocksByAddress :: (MM.MemWidth w)
                     => R.ISA i a w
                     -> R.BlockInfo i w arch
                     -> IM.IntervalMap (MM.MemAddr w) (R.ConcreteBlock i w)
indexBlocksByAddress isa bi = F.foldl' indexBlock IM.empty (R.biBlocks bi)
  where
    indexBlock m b =
      let iaddrs = fmap (MM.absoluteAddr . R.absoluteAddress . snd) (R.instructionAddresses isa b)
      in IM.insert (IM.ClosedInterval (minimum iaddrs) (maximum iaddrs)) b m

-- | Look up the basic block containing the given address.
--
-- The 'Word64' is converted to a 'MM.MemAddr'.
blocksContaining :: BinaryAnalysisResult s o i a w arch -> MM.MemAddr w -> [R.ConcreteBlock i w]
blocksContaining bar addr = IM.elems (IM.containing (rBlockMap bar) addr)
