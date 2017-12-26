{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- | Abstractions over program representations
--
-- These definitions are designed to allow the Surveyor UI to abstractly handle
-- different architectures, building up abstractions over functions, blocks,
-- instructions, and operands.
module Surveyor.Architecture (
  Architecture(..),
  SomeResult(..),
  mkPPC32Result,
  mkPPC64Result,
  mkX86Result
  ) where

import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Text as T
import           Data.Void

import qualified Dismantle.PPC as DPPC
import qualified Flexdis86 as FD
import qualified Renovate as R
import qualified Renovate.Arch.PPC as PPC
import qualified Renovate.Arch.X86_64 as X86

import           Surveyor.BinaryAnalysisResult

data SomeResult s where
  SomeResult :: (Architecture st arch s) => AnalysisResult st arch s -> SomeResult s

class Architecture (st :: *) (arch :: *) s where
  data AnalysisResult st arch s :: *
  data Block st arch :: *
  data Instruction st arch :: *
  data Operand st arch :: *
  data Address st arch :: *

  summarizeResult :: AnalysisResult st arch s -> [(T.Text, T.Text)]

mkPPC32Result :: BinaryAnalysisResult s PPC.Instruction a 32 PPC.PPC32
              -> SomeResult s
mkPPC32Result = SomeResult . PPC32AnalysisResult

mkPPC64Result :: BinaryAnalysisResult s PPC.Instruction a 64 PPC.PPC64
              -> SomeResult s
mkPPC64Result = SomeResult . PPC64AnalysisResult

mkX86Result :: BinaryAnalysisResult s X86.Instruction a 64 X86.X86_64
            -> SomeResult s
mkX86Result = SomeResult . X86AnalysisResult

summarizeRenovate :: BinaryAnalysisResult s i a w arch -> [(T.Text, T.Text)]
summarizeRenovate bar =
  [ ("Discovered Functions", T.pack (show (length (R.biFunctionEntries binfo))))
  , ("Discovered Blocks", T.pack (show (length (R.biBlocks binfo))))
  ]
  where
    binfo = rBlockInfo bar

instance Architecture Void Void s where
  data AnalysisResult Void Void s = VoidAnalysisResult Void
  data Block Void Void = VoidBlock Void
  data Instruction Void Void = VoidInstruction Void
  data Operand Void Void = VoidOperand Void
  data Address Void Void = VoidAddress Void
  summarizeResult (VoidAnalysisResult v) = absurd v

instance Architecture (BinaryAnalysisResult s PPC.Instruction a 32 PPC.PPC32) PPC.PPC32 s where
  data AnalysisResult (BinaryAnalysisResult s PPC.Instruction a 32 PPC.PPC32) PPC.PPC32 s =
    PPC32AnalysisResult (BinaryAnalysisResult s PPC.Instruction a 32 PPC.PPC32)
  data Block (BinaryAnalysisResult s PPC.Instruction a 32 PPC.PPC32) PPC.PPC32 =
    PPC32Block (R.ConcreteBlock PPC.Instruction 32)
  data Instruction (BinaryAnalysisResult s PPC.Instruction a 32 PPC.PPC32) PPC.PPC32 =
    PPC32Instruction (PPC.Instruction ())
  data Operand (BinaryAnalysisResult s PPC.Instruction a 32 PPC.PPC32) PPC.PPC32 =
    PPC32Operand (Some DPPC.Operand)

  summarizeResult (PPC32AnalysisResult bar) = summarizeRenovate bar

instance Architecture (BinaryAnalysisResult s PPC.Instruction a 64 PPC.PPC64) PPC.PPC64 s where
  data AnalysisResult (BinaryAnalysisResult s PPC.Instruction a 64 PPC.PPC64) PPC.PPC64 s =
    PPC64AnalysisResult (BinaryAnalysisResult s PPC.Instruction a 64 PPC.PPC64)
  data Block (BinaryAnalysisResult s PPC.Instruction a 64 PPC.PPC64) PPC.PPC64 =
    PPC64Block (R.ConcreteBlock PPC.Instruction 64)
  data Instruction (BinaryAnalysisResult s PPC.Instruction a 64 PPC.PPC64) PPC.PPC64 =
    PPC64Instruction (PPC.Instruction ())
  data Operand (BinaryAnalysisResult s PPC.Instruction a 64 PPC.PPC64) PPC.PPC64 =
    PPC64Operand (Some DPPC.Operand)

  summarizeResult (PPC64AnalysisResult bar) = summarizeRenovate bar

instance Architecture (BinaryAnalysisResult s X86.Instruction a 64 X86.X86_64) X86.X86_64 s where
  data AnalysisResult (BinaryAnalysisResult s X86.Instruction a 64 X86.X86_64) X86.X86_64 s =
    X86AnalysisResult (BinaryAnalysisResult s X86.Instruction a 64 X86.X86_64)
  data Block (BinaryAnalysisResult s X86.Instruction a 64 X86.X86_64) X86.X86_64 =
    X86Block (R.ConcreteBlock X86.Instruction 64)
  data Instruction (BinaryAnalysisResult s X86.Instruction a 64 X86.X86_64) X86.X86_64 =
    X86Instruction (X86.Instruction ())
  data Operand (BinaryAnalysisResult s X86.Instruction a 64 X86.X86_64) X86.X86_64 =
    X86Operand FD.Value FD.OperandType

  summarizeResult (X86AnalysisResult bar) = summarizeRenovate bar
