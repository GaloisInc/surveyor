{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Surveyor.Core.Mode (
  UIMode(..),
  UIKind(..),
  NormalK,
  MiniBufferK,
  SomeUIMode(..),
  prettyMode
  ) where

import           Data.Kind ( Type )
import           Data.Maybe ( isJust )
import qualified Data.Parameterized.Classes as PC
import qualified Data.Parameterized.Nonce as PN
import qualified Data.Parameterized.TH.GADT as PT
import qualified Prettyprinter as PP

import           Surveyor.Core.IRRepr ( IRRepr )

data UIKind = MiniBufferK
            | NormalK

type MiniBufferK = 'MiniBufferK
type NormalK = 'NormalK

data UIMode s k where
  -- | A window containing the history of diagnostic information
  Diags :: UIMode s NormalK
  -- | Summary information returned by the binary analysis
  Summary :: UIMode s NormalK
  -- | A list of all of the discovered functions (which allows for
  -- drilling down and displaying blocks)
  FunctionSelector :: UIMode s NormalK
  -- | A selector list for blocks that are the result of a search (based on the
  -- sBlockList in the State)
  BlockSelector :: UIMode s NormalK
  -- | View a block
  BlockViewer :: PN.Nonce s (arch :: Type) -> IRRepr arch ir -> UIMode s NormalK
  -- | View a function
  FunctionViewer :: PN.Nonce s (arch :: Type) -> IRRepr arch ir -> UIMode s NormalK
  -- | View the semantics for an individual selected base IR instruction
  SemanticsViewer :: UIMode s NormalK
  -- | A UI for setting up the basic configuration of the symbolic execution
  -- engine (includes solver selection and float mode selection), running the
  -- symbolic execution engine, and inspecting results.
  SymbolicExecutionManager :: UIMode s NormalK
  -- | An interactive widget that takes focus and accepts all
  -- keystrokes except for C-g
  MiniBuffer :: UIMode s NormalK -> UIMode s MiniBufferK

prettyMode :: UIMode s NormalK -> PP.Doc ann
prettyMode m =
  case m of
    Diags -> PP.pretty "Diagnostics"
    Summary -> PP.pretty "Summary"
    FunctionSelector -> PP.pretty "Function Selector"
    BlockSelector -> PP.pretty "Block Selector"
    BlockViewer _nonce repr -> PP.pretty "Block Viewer" <> PP.brackets (PP.pretty repr)
    FunctionViewer _nonce repr -> PP.pretty "Function Viewer" <> PP.brackets (PP.pretty repr)
    SemanticsViewer -> PP.pretty "Semantics Viewer"
    SymbolicExecutionManager -> PP.pretty "Symbolic Execution Manager"

data SomeUIMode s where
  SomeMiniBuffer :: UIMode s MiniBufferK -> SomeUIMode s
  SomeUIMode :: UIMode s NormalK -> SomeUIMode s

$(return [])

instance PC.TestEquality (UIMode s) where
  testEquality = $(PT.structuralTypeEquality [t| UIMode |]
                   [ (PT.TypeApp (PT.TypeApp (PT.ConType [t|PN.Nonce |]) PT.AnyType) PT.AnyType, [|PC.testEquality|])
                   , (PT.TypeApp (PT.TypeApp (PT.ConType [t| UIMode |]) PT.AnyType) PT.AnyType, [|PC.testEquality|])
                   , (PT.TypeApp (PT.TypeApp (PT.ConType [t| IRRepr |]) PT.AnyType) PT.AnyType, [|PC.testEquality|])
                   ])

instance PC.OrdF (UIMode s) where
  compareF = $(PT.structuralTypeOrd [t| UIMode |]
                   [ (PT.TypeApp (PT.TypeApp (PT.ConType [t|PN.Nonce |]) PT.AnyType) PT.AnyType, [|PC.compareF|])
                   , (PT.TypeApp (PT.TypeApp (PT.ConType [t| UIMode |]) PT.AnyType) PT.AnyType, [|PC.compareF|])
                   , (PT.TypeApp (PT.TypeApp (PT.ConType [t| IRRepr |]) PT.AnyType) PT.AnyType, [|PC.compareF|])
                   ])

deriving instance Eq (SomeUIMode s)
deriving instance Ord (SomeUIMode s)

instance Eq (UIMode s k) where
  u1 == u2 = isJust (PC.testEquality u1 u2)

instance Ord (UIMode s k) where
  compare u1 u2 = PC.toOrdering (PC.compareF u1 u2)
