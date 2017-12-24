{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
module Surveyor.Events ( Events(..) ) where

import qualified Control.Exception as X
import qualified Data.ElfEdit as E
import           Data.Int ( Int64 )
import           Data.Parameterized.Some ( Some )
import qualified Data.Text as T
import           Data.Word ( Word64 )

import qualified Data.Macaw.Memory as MM
import qualified Renovate as R

import qualified Brick.Command as C
import           Surveyor.BinaryAnalysisResult ( BinaryAnalysisResultWrapper )
import qualified Surveyor.EchoArea as EA

data Events s where
  ErrorLoadingELFHeader :: Int64 -> String -> Events s
  ErrorLoadingELF :: (Eq (E.ElfWordType n), Num (E.ElfWordType n), Show (E.ElfWordType n))
                  => [E.ElfParseError n] -> Events s
  AnalysisFailure :: X.SomeException -> Events s
  AnalysisFinished :: BinaryAnalysisResultWrapper s -> [R.Diagnostic] -> Events s
  BlockDiscovered :: (MM.MemWidth w) => MM.MemAddr w -> Events s
  AnalysisProgress :: MM.MemAddr w -> BinaryAnalysisResultWrapper s -> Events s
  FindBlockContaining :: Word64 -> Events s
  DescribeCommand :: Some (C.Command a r) -> Events s
  EchoText :: T.Text -> Events s
  UpdateEchoArea :: EA.EchoArea -> Events s
  ShowSummary :: Events s
  ShowDiagnostics :: Events s
  Exit :: Events s
