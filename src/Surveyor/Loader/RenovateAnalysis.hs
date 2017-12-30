module Surveyor.Loader.RenovateAnalysis (
  analysis
  ) where

import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.Nonce as NG
import qualified Data.Macaw.Memory as MM
import qualified Lang.Crucible.Solver.SimpleBackend as SB
import qualified Renovate as R
import qualified SemMC.Formula as F

import qualified Surveyor.Architecture as A
import           Surveyor.BinaryAnalysisResult

analysis :: (A.Architecture arch s, MM.MemWidth w)
         => (BinaryAnalysisResult s o i a w arch -> A.SomeResult s)
         -> NG.Nonce s arch
         -> Maybe (MapF.MapF o (F.ParameterizedFormula (SB.SimpleBackend s) arch))
         -> R.ISA i a w
         -> MM.Memory w
         -> R.BlockInfo i w arch
         -> A.SomeResult s
analysis con nonce semantics isa mem bi = con r
  where
    r = BinaryAnalysisResult { rBlockInfo = bi
                             , rMemory = mem
                             , rISA = isa
                             , rBlockMap = indexBlocksByAddress isa mem bi
                             , rNonce = nonce
                             , rSemantics = semantics
                             }
