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
         => R.ISA arch
         -> (BinaryAnalysisResult s o arch -> A.SomeResult s arch)
         -> NG.Nonce s arch
         -> Maybe (MapF.MapF o (F.ParameterizedFormula (SB.SimpleBackend s) arch))
         -> R.RewriteEnv arch
         -> MBL.LoadedBinary arch binFmt
         -> A.SomeResult s arch
analysis isa con nonce semantics env loadedBinary = con r
  where
    r = BinaryAnalysisResult { rBlockInfo = R.envBlockInfo env
                             , rLoadedBinary = loadedBinary
                             , rISA = isa
                             , rBlockMap = indexBlocksByAddress isa (R.envBlockInfo env)
                             , rNonce = nonce
                             , rSemantics = semantics
                             }
