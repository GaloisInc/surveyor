{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
module Surveyor.Core.Mode (
  UIMode(..),
  UIKind(..),
  NormalK,
  MiniBufferK,
  SomeUIMode(..),
  prettyMode
  ) where

import           Data.Parameterized.Classes
import qualified Data.Text as T

data UIKind = MiniBufferK
            | NormalK

type MiniBufferK = 'MiniBufferK
type NormalK = 'NormalK

data UIMode s where
  Diags :: UIMode NormalK
  -- ^ A window containing the history of diagnostic information
  Summary :: UIMode NormalK
  -- ^ Summary information returned by the binary analysis
  FunctionSelector :: UIMode NormalK
  -- ^ A list of all of the discovered functions (which allows for
  -- drilling down and displaying blocks)
  BlockSelector :: UIMode NormalK
  -- ^ A selector list for blocks that are the result of a search (based on the
  -- sBlockList in the State)
  BlockViewer :: UIMode NormalK
  -- ^ View a block
  FunctionViewer :: UIMode NormalK
  -- ^ View a function
  MiniBuffer :: UIMode NormalK -> UIMode MiniBufferK
  -- ^ An interactive widget that takes focus and accepts all
  -- keystrokes except for C-g

prettyMode :: UIMode NormalK -> T.Text
prettyMode m =
  case m of
    Diags -> "Diagnostics"
    Summary -> "Summary"
    FunctionSelector -> "Function Selector"
    BlockSelector -> "Block Selector"
    BlockViewer -> "Block Viewer"
    FunctionViewer -> "Function Viewer"

data SomeUIMode where
  SomeMiniBuffer :: UIMode MiniBufferK -> SomeUIMode
  SomeUIMode :: UIMode NormalK -> SomeUIMode

deriving instance Eq SomeUIMode
deriving instance Ord SomeUIMode

deriving instance Eq (UIMode s)
deriving instance Ord (UIMode s)
deriving instance Show (UIMode s)

instance TestEquality UIMode where
  testEquality Diags Diags = Just Refl
  testEquality Summary Summary = Just Refl
  testEquality FunctionSelector FunctionSelector = Just Refl
  testEquality BlockSelector BlockSelector = Just Refl
  testEquality FunctionViewer FunctionViewer = Just Refl
  testEquality BlockViewer BlockViewer = Just Refl
  testEquality (MiniBuffer a) (MiniBuffer b) = do
    Refl <- testEquality a b
    return Refl
  testEquality _ _ = Nothing
