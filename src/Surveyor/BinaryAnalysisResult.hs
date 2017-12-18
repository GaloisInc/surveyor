{-# LANGUAGE ExistentialQuantification #-}
module Surveyor.BinaryAnalysisResult (
  BinaryAnalysisResult(..)
  ) where

import qualified Data.Macaw.Memory as MM
import qualified Renovate as R

data BinaryAnalysisResult = forall i a w arch .
  (MM.MemWidth w) =>
  BinaryAnalysisResult { rBlockInfo :: R.BlockInfo i w arch
                       , rMemory :: MM.Memory w
                       , rISA :: R.ISA i a w
                       }
