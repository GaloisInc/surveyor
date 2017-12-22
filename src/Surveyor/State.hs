{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module Surveyor.State (
  State(..),
  S(..),
  FunctionListEntry(..),
  AppState(..),
  Names(..)
  ) where

import qualified Brick.Widgets.List as B
import qualified Data.Sequence as Seq
import qualified Data.Text as T

import qualified Data.Macaw.Memory as MM
import qualified Renovate as R

import           Surveyor.BinaryAnalysisResult ( BinaryAnalysisResult(..) )
import           Surveyor.Events ( Events )
import qualified Surveyor.Minibuffer as MB
import           Surveyor.Mode

data State where
  State :: (MM.MemWidth w) => S i a w arch -> State

data S i a w arch =
  S { sInputFile :: Maybe FilePath
    , sBinaryInfo :: Maybe (BinaryAnalysisResult i a w arch)
    , sDiagnosticLog :: Seq.Seq T.Text
    , sUIMode :: SomeUIMode
    , sFunctionList :: B.List Names (FunctionListEntry w)
    , sMinibuffer :: MB.Minibuffer MB.Argument MB.TypeRepr T.Text Names
    , sAppState :: AppState
    , sEmitEvent :: Events -> IO ()
    }

data FunctionListEntry w = FLE (R.ConcreteAddress w) T.Text Int

data AppState = Loading
              | Ready
              | AwaitingFile

data Names = DiagnosticView
           | DiagnosticContent
           | FunctionList
           | MinibufferEditor
           | MinibufferCompletionList
  deriving (Eq, Ord, Show)
