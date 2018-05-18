{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
module Surveyor.Events ( Events(..) ) where

import qualified Control.Exception as X
import qualified Data.ElfEdit as E
import           Data.Int ( Int64 )
import qualified Data.Parameterized.Nonce as PN
import           Data.Parameterized.Some ( Some )
import qualified Data.Text as T

import qualified Renovate as R

import qualified Surveyor.Core.Command as C
import qualified Surveyor.Architecture as A
import qualified Surveyor.Widget.EchoArea as EA

data Events s where
  -- Loading events
  ErrorLoadingELFHeader :: Int64 -> String -> Events s
  ErrorLoadingELF :: (Eq (E.ElfWordType n), Num (E.ElfWordType n), Show (E.ElfWordType n), Integral (E.ElfWordType n))
                  => [E.ElfParseError n] -> Events s
  ErrorLoadingLLVM :: String -> Events s
  AnalysisFailure :: X.SomeException -> Events s
  AnalysisFinished :: A.SomeResult s arch -> [R.Diagnostic] -> Events s
  AnalysisProgress :: A.SomeResult s arch -> Events s

  LoadELF :: FilePath -> Events s
  LoadLLVM :: FilePath -> Events s
  LoadJAR :: FilePath -> Events s
  -- | Attempt to load a file by detecting its type automatically
  LoadFile :: FilePath -> Events s

  -- Function-related events
  FindFunctionsContaining :: PN.Nonce s arch -> Maybe (A.Address arch s) -> Events s
  ListFunctions :: PN.Nonce s arch -> [A.FunctionHandle arch s] -> Events s
  ViewFunction :: PN.Nonce s arch -> A.FunctionHandle arch s -> Events s

  -- Block-related events
  FindBlockContaining :: PN.Nonce s arch -> A.Address arch s -> Events s
  ListBlocks :: PN.Nonce s arch -> [A.Block arch s] -> Events s
  ViewBlock :: PN.Nonce s arch -> A.Block arch s -> Events s

  -- Informational messages
  DescribeCommand :: Some (C.Command (Events s) st a r) -> Events s
  EchoText :: !T.Text -> Events s
  UpdateEchoArea :: !EA.EchoArea -> Events s
  LogDiagnostic :: !T.Text -> Events s

  -- UI Modes
  ShowSummary :: Events s
  ShowDiagnostics :: Events s
  OpenMinibuffer :: Events s

  -- Exit
  Exit :: Events s
