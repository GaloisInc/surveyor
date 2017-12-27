module Surveyor.Loader.RenovateAnalysis (
  analysis
  ) where

import qualified Data.Parameterized.Nonce as NG
import qualified Data.Macaw.Memory as MM
import qualified Renovate as R

import qualified Surveyor.Architecture as A
import           Surveyor.BinaryAnalysisResult

analysis :: (A.Architecture arch s, MM.MemWidth w)
         => (BinaryAnalysisResult s i a w arch -> A.SomeResult s)
         -> (NG.Nonce s w, NG.Nonce s i, NG.Nonce s arch)
         -> R.ISA i a w
         -> MM.Memory w
         -> R.BlockInfo i w arch
         -> A.SomeResult s
analysis con nonces isa mem bi = con r
  where
    r = BinaryAnalysisResult { rBlockInfo = bi
                             , rMemory = mem
                             , rISA = isa
                             , rBlockMap = indexBlocksByAddress isa mem bi
                             , rNonces = nonces
                             }
