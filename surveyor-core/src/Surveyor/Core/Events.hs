{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
module Surveyor.Core.Events ( Events(..), LogLevel(..) ) where

import qualified Control.Exception as X
import qualified Control.NF as NF
import qualified Data.ElfEdit as E
import           Data.Int ( Int64 )
import           Data.Kind ( Type )
import qualified Data.Parameterized.Nonce as PN
import qualified Data.Text as T

import qualified Renovate as R

import qualified Surveyor.Core.Architecture as A
import qualified Surveyor.Core.Command as C
import qualified Surveyor.Core.IRRepr as IR

data LogLevel = LogDebug
              | LogInfo
              | LogWarning
              | LogError
              | LogFatal
              deriving (Show, Eq, Ord)

data Events s st where
  -- Loading events
  ErrorLoadingELFHeader :: Int64 -> String -> Events s st
  ErrorLoadingELF :: [E.ElfParseError] -> Events s st
  ErrorLoadingLLVM :: String -> Events s st
  AnalysisFailure :: X.SomeException -> Events s st
  AnalysisFinished :: A.SomeResult s arch -> [R.Diagnostic] -> Events s st
  AnalysisProgress :: A.SomeResult s arch -> Events s st

  LoadELF :: FilePath -> Events s st
  LoadLLVM :: FilePath -> Events s st
  LoadJAR :: FilePath -> Events s st
  -- | Attempt to load a file by detecting its type automatically
  LoadFile :: FilePath -> Events s st

  -- Context manipulation
  PushContext :: (A.ArchConstraints ir s) => PN.Nonce s arch -> A.FunctionHandle arch s -> IR.IRRepr arch ir -> A.Block ir s -> Events s st
  ContextBack :: Events s st
  ContextForward :: Events s st

  -- Function-related events
  FindFunctionsContaining :: PN.Nonce s arch -> Maybe (A.Address arch s) -> Events s st
  ListFunctions :: PN.Nonce s arch -> [A.FunctionHandle arch s] -> Events s st
  ViewFunction :: PN.Nonce s (arch :: *) -> IR.IRRepr arch ir -> Events s st


  -- Block-related events
  FindBlockContaining :: PN.Nonce s arch -> A.Address arch s -> Events s st
  ListBlocks :: PN.Nonce s arch -> [A.Block arch s] -> Events s st
  ViewBlock :: PN.Nonce s arch -> IR.IRRepr arch ir -> Events s st
  ViewInstructionSemantics :: PN.Nonce s arch -> Events s st
  SelectNextInstruction :: PN.Nonce s (arch :: Type) -> Events s st
  SelectPreviousInstruction :: PN.Nonce s (arch :: Type) -> Events s st
  SelectNextOperand :: PN.Nonce s (arch :: Type) -> Events s st
  SelectPreviousOperand :: PN.Nonce s (arch :: Type) -> Events s st
  ResetInstructionSelection :: PN.Nonce s (arch :: Type) -> Events s st

  -- Informational messages
  DescribeCommand :: (C.CommandLike cmd) => C.SomeCommand cmd -> Events s st
  EchoText :: !T.Text -> Events s st
  ResetEchoArea :: Events s st
  LogDiagnostic :: Maybe LogLevel -> !T.Text -> Events s st
  DescribeKeys :: Events s st

  -- UI Modes
  ShowSummary :: Events s st
  ShowDiagnostics :: Events s st
  OpenMinibuffer :: Events s st

  -- Apply an arbitrary state update based on some value that has been computed
  -- asynchronously (to avoid blocking the event loop with an expensive
  -- computation).
  AsyncStateUpdate :: PN.Nonce s arch -> !(NF.NF a) -> (a -> st arch s -> st arch s) -> Events s st

  -- Exit
  Exit :: Events s st
