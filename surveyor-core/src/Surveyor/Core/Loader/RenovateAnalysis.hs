{-# LANGUAGE GADTs #-}
module Surveyor.Core.Loader.RenovateAnalysis (
  analysis
  ) where

import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.Nonce as NG
import qualified Data.Macaw.BinaryLoader as MBL
import qualified Data.Macaw.CFG as MM
import qualified Lang.Crucible.Backend.Simple as SB
import qualified Renovate as R
import qualified SemMC.Formula as F

import qualified Surveyor.Core.Architecture as A
import qualified Surveyor.Core.BinaryAnalysisResult as BAR

analysis :: (A.Architecture arch s, MM.MemWidth w, w ~ MM.ArchAddrWidth arch)
         => R.ISA arch
         -> (BAR.BinaryAnalysisResult s o arch -> A.SomeResult s arch)
         -> NG.Nonce s arch
         -> Maybe (MapF.MapF o (F.ParameterizedFormula (SB.SimpleBackend s) arch))
         -> R.AnalyzeEnv arch
         -> MBL.LoadedBinary arch binFmt
         -> IO (A.SomeResult s arch)
analysis isa con nonce semantics env loadedBinary = return (con r)
  where
    r = BAR.BinaryAnalysisResult { BAR.rBlockInfo = R.envBlockInfo (R.aeRewriteEnv env)
                                 , BAR.rLoadedBinary = loadedBinary
                                 , BAR.rISA = isa
                                 , BAR.rBlockMap = BAR.indexBlocksByAddress isa (R.envBlockInfo (R.aeRewriteEnv env))
                                 , BAR.rNonce = nonce
                                 , BAR.rSemantics = semantics
                                 }
