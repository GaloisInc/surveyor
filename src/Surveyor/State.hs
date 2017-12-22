{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module Surveyor.State (
  State(..),
  S(..),
  FunctionListEntry(..),
  AppState(..),
  UIMode(..),
  UIKind(..),
  NormalK,
  MiniBufferK,
  SomeUIMode(..),
  Names(..)
  ) where

import qualified Brick.Widgets.List as B
import           Data.Parameterized.Classes
import qualified Data.Sequence as Seq
import qualified Data.Text as T

import qualified Data.Macaw.Memory as MM
import qualified Renovate as R

import           Surveyor.BinaryAnalysisResult ( BinaryAnalysisResult(..)
                                               )
import qualified Surveyor.Minibuffer as MB

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
    }

data FunctionListEntry w = FLE (R.ConcreteAddress w) T.Text Int

data AppState = Loading
              | Ready
              | AwaitingFile

data UIKind = MiniBufferK
            | NormalK

type MiniBufferK = 'MiniBufferK
type NormalK = 'NormalK

data UIMode s where
  Diags :: UIMode NormalK
  -- ^ A window containing the history of diagnostic information
  Summary :: UIMode NormalK
  -- ^ Summary information returned by the binary analysis
  ListFunctions :: UIMode NormalK
  -- ^ A list of all of the discovered functions (which allows for
  -- drilling down and displaying blocks)
  MiniBuffer :: UIMode NormalK -> UIMode MiniBufferK
  -- ^ An interactive widget that takes focus and accepts all
  -- keystrokes except for C-g

data SomeUIMode where
  SomeMiniBuffer :: UIMode MiniBufferK -> SomeUIMode
  SomeUIMode :: UIMode NormalK -> SomeUIMode

deriving instance Eq SomeUIMode

deriving instance Eq (UIMode s)
deriving instance Ord (UIMode s)
deriving instance Show (UIMode s)

instance TestEquality UIMode where
  testEquality Diags Diags = Just Refl
  testEquality Summary Summary = Just Refl
  testEquality ListFunctions ListFunctions = Just Refl
  testEquality (MiniBuffer a) (MiniBuffer b) = do
    _ <- testEquality a b
    return Refl
  testEquality _ _ = Nothing

data Names = DiagnosticView
           | DiagnosticContent
           | FunctionList
           | MinibufferEditor
           | MinibufferCompletionList
  deriving (Eq, Ord, Show)
