{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
module Surveyor.Events ( Events(..) ) where

import qualified Control.Exception as X
import qualified Data.ElfEdit as E
import           Data.Int ( Int64 )
import           Data.Parameterized.Some ( Some )
import qualified Data.Text as T

import qualified Renovate as R

import qualified Brick.Command as C
import qualified Surveyor.Architecture as A
import qualified Surveyor.EchoArea as EA

data Events s where
  ErrorLoadingELFHeader :: Int64 -> String -> Events s
  ErrorLoadingELF :: (Eq (E.ElfWordType n), Num (E.ElfWordType n), Show (E.ElfWordType n))
                  => [E.ElfParseError n] -> Events s
  AnalysisFailure :: X.SomeException -> Events s
  AnalysisFinished :: A.SomeResult s -> [R.Diagnostic] -> Events s
  AnalysisProgress :: A.SomeResult s -> Events s
  FindBlockContaining :: A.Address arch s -> Events s
  DescribeCommand :: Some (C.Command a r) -> Events s
  EchoText :: T.Text -> Events s
  UpdateEchoArea :: EA.EchoArea -> Events s
  ShowSummary :: Events s
  ShowDiagnostics :: Events s
  OpenMinibuffer :: Events s
  Exit :: Events s
