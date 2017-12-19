{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module Surveyor.BinaryAnalysisResult (
  BinaryAnalysisResult(..),
  BinaryAnalysisResultWrapper(..)
  ) where

import qualified Data.Macaw.Memory as MM
import qualified Renovate as R

data BinaryAnalysisResultWrapper where
  BinaryAnalysisResultWrapper :: (MM.MemWidth w) => BinaryAnalysisResult i a w arch -> BinaryAnalysisResultWrapper

data BinaryAnalysisResult i a w arch =
  BinaryAnalysisResult { rBlockInfo :: R.BlockInfo i w arch
                       , rMemory :: MM.Memory w
                       , rISA :: R.ISA i a w
                       }
