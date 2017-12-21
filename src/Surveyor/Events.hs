{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Surveyor.Events ( Events(..) ) where

import qualified Control.Exception as X
import qualified Data.ElfEdit as E
import           Data.Int ( Int64 )

import qualified Data.Macaw.Memory as MM
import qualified Renovate as R

import           Surveyor.BinaryAnalysisResult ( BinaryAnalysisResultWrapper )

data Events where
  ErrorLoadingELFHeader :: Int64 -> String -> Events
  ErrorLoadingELF :: (Eq (E.ElfWordType n), Num (E.ElfWordType n), Show (E.ElfWordType n))
                  => [E.ElfParseError n] -> Events
  AnalysisFailure :: X.SomeException -> Events
  AnalysisFinished :: BinaryAnalysisResultWrapper -> [R.Diagnostic] -> Events
  BlockDiscovered :: (MM.MemWidth w) => MM.MemAddr w -> Events
  AnalysisProgress :: MM.MemAddr w -> BinaryAnalysisResultWrapper -> Events
