{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
module Surveyor.Core.IRRepr (
  IRRepr(..),
  Macaw,
  Crucible
  ) where

import           Data.Parameterized.Classes
import qualified Data.Parameterized.TH.GADT as PT
import qualified Prettyprinter as PP

data Macaw arch
data Crucible arch

data IRRepr arch ir where
  -- | A repr for a "base" representation (e.g., machine code, LLVM)
  BaseRepr :: IRRepr arch arch
  -- | A repr for a macaw view of another IR
  MacawRepr :: IRRepr arch (Macaw arch)
  -- | A repr for the Crucible view of another IR
  CrucibleRepr :: IRRepr arch (Crucible arch)

instance ShowF (IRRepr arch)

instance Show (IRRepr arch ir) where
  show rep =
    case rep of
      BaseRepr -> "base"
      MacawRepr -> "macaw"
      CrucibleRepr -> "crucible"

instance PP.Pretty (IRRepr arch ir) where
  pretty = PP.viaShow

$(return [])

instance TestEquality (IRRepr arch) where
  testEquality = $(PT.structuralTypeEquality [t| IRRepr |] [])

instance OrdF (IRRepr arch) where
  compareF = $(PT.structuralTypeOrd [t| IRRepr |] [])
