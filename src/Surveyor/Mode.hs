{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module Surveyor.Mode (
  UIMode(..),
  UIKind(..),
  NormalK,
  MiniBufferK,
  SomeUIMode(..)
  ) where

import Data.Parameterized.Classes

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
  BlockSelector :: UIMode NormalK
  -- ^ A selector list for blocks that are the result of a search (based on the
  -- sBlockList in the State)
  MiniBuffer :: UIMode NormalK -> UIMode MiniBufferK
  -- ^ An interactive widget that takes focus and accepts all
  -- keystrokes except for C-g

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
  testEquality ListFunctions ListFunctions = Just Refl
  testEquality BlockSelector BlockSelector = Just Refl
  testEquality (MiniBuffer a) (MiniBuffer b) = do
    _ <- testEquality a b
    return Refl
  testEquality _ _ = Nothing
