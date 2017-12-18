{-# LANGUAGE GADTs #-}
module Surveyor.Events ( Events(..) ) where

import qualified Control.Exception as X
import qualified Data.ElfEdit as E
import           Data.Int ( Int64 )

import qualified Renovate as R

import           Surveyor.BinaryAnalysisResult ( BinaryAnalysisResult )

data Events where
  ErrorLoadingELFHeader :: Int64 -> String -> Events
  ErrorLoadingELF :: [E.ElfParseError n] -> Events
  AnalysisFailure :: X.SomeException -> Events
  AnalysisFinished :: BinaryAnalysisResult -> [R.Diagnostic] -> Events
