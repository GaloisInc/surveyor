{-# LANGUAGE GADTs #-}
module Surveyor.Loader.RenovateAnalysis (
  analysis
  ) where

import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.Nonce as NG
import qualified Data.Macaw.BinaryLoader as MBL
import qualified Data.Macaw.CFG as MM
import qualified Lang.Crucible.Backend.Simple as SB
import qualified Renovate as R
import qualified SemMC.Formula as F

import qualified Surveyor.Architecture as A
import           Surveyor.BinaryAnalysisResult

analysis :: (A.Architecture arch s, MM.MemWidth w, w ~ MM.ArchAddrWidth arch)
         => (BinaryAnalysisResult s o arch -> A.SomeResult s arch)
         -> NG.Nonce s arch
         -> Maybe (MapF.MapF o (F.ParameterizedFormula (SB.SimpleBackend s) arch))
         -> R.ISA arch
         -> MBL.LoadedBinary arch binFmt
         -> R.BlockInfo arch
         -> A.SomeResult s arch
analysis con nonce semantics isa loadedBinary bi = con r
  where
    r = BinaryAnalysisResult { rBlockInfo = bi
                             , rLoadedBinary = loadedBinary
                             , rISA = isa
                             , rBlockMap = indexBlocksByAddress isa bi
                             , rNonce = nonce
                             , rSemantics = semantics
                             }
