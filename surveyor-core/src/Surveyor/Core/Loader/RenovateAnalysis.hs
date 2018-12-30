{-# LANGUAGE GADTs #-}
module Surveyor.Core.Loader.RenovateAnalysis (
  analysis
  ) where

import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.Nonce as NG
import qualified Data.Macaw.CFG as MM
import qualified Lang.Crucible.Backend.Simple as SB
import qualified Renovate as R
import qualified SemMC.Formula as F

import qualified Surveyor.Core.Architecture as A
import qualified Surveyor.Core.BinaryAnalysisResult as BAR

analysis :: ( A.Architecture arch s
            , MM.MemWidth w
            , w ~ MM.ArchAddrWidth arch
            , R.HasAnalysisEnv env
            )
         => SB.SimpleBackend s fs
         -> R.ISA arch
         -> (BAR.BinaryAnalysisResult s o arch -> A.SomeResult s arch)
         -> NG.NonceGenerator IO s
         -> NG.Nonce s arch
         -> Maybe (MapF.MapF o (F.ParameterizedFormula (SB.SimpleBackend s fs) arch))
         -> env arch binFmt
         -> IO (A.SomeResult s arch)
analysis sym isa con ng nonce semantics env = return (con r)
  where
    r = BAR.BinaryAnalysisResult { BAR.rBlockInfo = R.analysisBlockInfo env
                                 , BAR.rLoadedBinary = R.analysisLoadedBinary env
                                 , BAR.rISA = isa
                                 , BAR.rAddressIndex = BAR.indexAddresses isa (R.analysisBlockInfo env)
                                 , BAR.rNonce = nonce
                                 , BAR.rSemantics = semantics
                                 , BAR.rSym = sym
                                 , BAR.rNonceGen = ng
                                 }
