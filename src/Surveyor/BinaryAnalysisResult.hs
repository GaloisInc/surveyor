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
import qualified Data.Parameterized.Nonce as NG
import qualified Renovate as R

data BinaryAnalysisResult s i a w arch =
  BinaryAnalysisResult { rBlockInfo :: R.BlockInfo i w arch
                       , rMemory :: MM.Memory w
                       , rISA :: R.ISA i a w
                       , rBlockMap :: IM.IntervalMap (MM.MemAddr w) (R.ConcreteBlock i w)
                       , rNonces :: (NG.Nonce s w, NG.Nonce s i, NG.Nonce s arch)
                       }

indexBlocksByAddress :: (MM.MemWidth w)
                     => R.ISA i a w
                     -> MM.Memory w
                     -> R.BlockInfo i w arch
                     -> IM.IntervalMap (MM.MemAddr w) (R.ConcreteBlock i w)
indexBlocksByAddress isa mem bi = F.foldl' indexBlock IM.empty (R.biBlocks bi)
  where
    indexBlock m b =
      let iaddrs = fmap (MM.absoluteAddr . R.absoluteAddress . snd) (R.instructionAddresses isa mem b)
      in IM.insert (IM.OpenInterval (minimum iaddrs) (maximum iaddrs)) b m

-- | Look up the basic block containing the given address.
--
-- The 'Word64' is converted to a 'MM.MemAddr'.
blocksContaining :: BinaryAnalysisResult s i a w arch -> MM.MemAddr w -> [R.ConcreteBlock i w]
blocksContaining bar addr = IM.elems (IM.containing (rBlockMap bar) addr)
