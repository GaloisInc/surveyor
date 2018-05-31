{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
module Surveyor.Core.Events ( Events(..) ) where

import qualified Control.Exception as X
import qualified Control.Once as O
import qualified Data.ElfEdit as E
import           Data.Int ( Int64 )
import qualified Data.Parameterized.Nonce as PN
import           Data.Parameterized.Some ( Some )
import qualified Data.Text as T

import qualified Renovate as R

import qualified Surveyor.Core.Architecture as A
import qualified Surveyor.Core.Command as C

data Events st s where
  -- Loading events
  ErrorLoadingELFHeader :: Int64 -> String -> Events s st
  ErrorLoadingELF :: (Eq (E.ElfWordType n), Num (E.ElfWordType n), Show (E.ElfWordType n), Integral (E.ElfWordType n))
                  => [E.ElfParseError n] -> Events s st
  ErrorLoadingLLVM :: String -> Events s st
  AnalysisFailure :: X.SomeException -> Events s st
  AnalysisFinished :: A.SomeResult s arch -> [R.Diagnostic] -> Events s st
  AnalysisProgress :: A.SomeResult s arch -> Events s st

  LoadELF :: FilePath -> Events s st
  LoadLLVM :: FilePath -> Events s st
  LoadJAR :: FilePath -> Events s st
  -- | Attempt to load a file by detecting its type automatically
  LoadFile :: FilePath -> Events s st

  -- Function-related events
  FindFunctionsContaining :: PN.Nonce s arch -> Maybe (A.Address arch s) -> Events s st
  ListFunctions :: PN.Nonce s arch -> [A.FunctionHandle arch s] -> Events s st
  ViewFunction :: PN.Nonce s arch -> A.FunctionHandle arch s -> Events s st

  -- Block-related events
  FindBlockContaining :: PN.Nonce s arch -> A.Address arch s -> Events s st
  ListBlocks :: PN.Nonce s arch -> [A.Block arch s] -> Events s st
  ViewBlock :: PN.Nonce s arch -> A.Block arch s -> Events s st

  -- Informational messages
  DescribeCommand :: Some (C.Command (Events s st) cmdState a r) -> Events s st
  EchoText :: !T.Text -> Events s st
  ResetEchoArea :: Events s st
  LogDiagnostic :: !T.Text -> Events s st

  -- UI Modes
  ShowSummary :: Events s st
  ShowDiagnostics :: Events s st
  OpenMinibuffer :: Events s st

  -- Apply an arbitrary state update based on some value that has been computed
  -- asynchronously (to avoid blocking the event loop with an expensive
  -- computation).
  AsyncStateUpdate :: PN.Nonce s arch -> !(O.Once a) -> (a -> st arch s -> st arch s) -> Events s st

  -- Exit
  Exit :: Events s st
