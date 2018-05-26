{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module Surveyor.Core.BinaryAnalysisResult (
  BinaryAnalysisResult(..),
  indexBlocksByAddress,
  blocksContaining
  ) where

import qualified Data.Foldable as F
import qualified Data.IntervalMap as IM
import qualified Data.Macaw.BinaryLoader as MBL
import qualified Data.Macaw.CFG as MC
import qualified Data.Macaw.Memory as MM
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.Nonce as NG
import qualified Lang.Crucible.Backend.Simple as SB
import qualified Renovate as R
import qualified SemMC.Formula as F

data BinaryAnalysisResult s o arch =
  forall binFmt .
  BinaryAnalysisResult { rBlockInfo :: !(R.BlockInfo arch)
                       , rLoadedBinary :: !(MBL.LoadedBinary arch binFmt)
                       , rISA :: R.ISA arch
                       , rBlockMap :: !(IM.IntervalMap (MM.MemAddr (MC.ArchAddrWidth arch)) (R.ConcreteBlock arch))
                       , rNonce :: NG.Nonce s arch
                       , rSemantics :: Maybe (MapF.MapF o (F.ParameterizedFormula (SB.SimpleBackend s) arch))
                       }

indexBlocksByAddress :: (MM.MemWidth (MC.ArchAddrWidth arch))
                     => R.ISA arch
                     -> R.BlockInfo arch
                     -> IM.IntervalMap (MM.MemAddr (MC.ArchAddrWidth arch)) (R.ConcreteBlock arch)
indexBlocksByAddress isa bi = F.foldl' indexBlock IM.empty (R.biBlocks bi)
  where
    indexBlock m b =
      let iaddrs = fmap (MM.absoluteAddr . R.absoluteAddress . snd) (R.instructionAddresses isa b)
      in IM.insert (IM.ClosedInterval (minimum iaddrs) (maximum iaddrs)) b m

-- | Look up the basic block containing the given address.
--
-- The 'Word64' is converted to a 'MM.MemAddr'.
blocksContaining :: BinaryAnalysisResult s o arch -> MM.MemAddr (MC.ArchAddrWidth arch) -> [R.ConcreteBlock arch]
blocksContaining bar addr = IM.elems (IM.containing (rBlockMap bar) addr)
