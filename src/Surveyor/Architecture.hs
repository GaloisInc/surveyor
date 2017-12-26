{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

import qualified Dismantle.PPC as DPPC
import qualified Flexdis86 as FD
import qualified Renovate as R
import qualified Renovate.Arch.PPC as PPC
import qualified Renovate.Arch.X86_64 as X86

import           Surveyor.BinaryAnalysisResult

data SomeResult s where
  SomeResult :: (Architecture st arch) => AnalysisResult st arch s -> SomeResult s

class Architecture (st :: *) (arch :: *) where
  data AnalysisResult st arch :: * -> *
  data Block s arch :: *
  data Instruction s arch :: *
  data Operand s arch :: *
  data Address s arch :: *

mkPPC32Result :: BinaryAnalysisResult s PPC.Instruction a 32 PPC.PPC32
              -> AnalysisResult (BinaryAnalysisResult s PPC.Instruction a 32 PPC.PPC32) PPC.PPC32 s
mkPPC32Result = PPC32AnalysisResult

mkPPC64Result :: BinaryAnalysisResult s PPC.Instruction a 64 PPC.PPC64
              -> AnalysisResult (BinaryAnalysisResult s PPC.Instruction a 64 PPC.PPC64) PPC.PPC64 s
mkPPC64Result = PPC64AnalysisResult

mkX86Result :: BinaryAnalysisResult s X86.Instruction a 64 X86.X86_64
            -> AnalysisResult (BinaryAnalysisResult s X86.Instruction a 64 X86.X86_64) X86.X86_64 s
mkX86Result = X86AnalysisResult

instance Architecture (BinaryAnalysisResult s PPC.Instruction a 32 PPC.PPC32) PPC.PPC32 where
  data AnalysisResult (BinaryAnalysisResult s PPC.Instruction a 32 PPC.PPC32) PPC.PPC32 s =
    PPC32AnalysisResult (BinaryAnalysisResult s PPC.Instruction a 32 PPC.PPC32)
  data Block (BinaryAnalysisResult s PPC.Instruction a 32 PPC.PPC32) PPC.PPC32 =
    PPC32Block (R.ConcreteBlock PPC.Instruction 32)
  data Instruction (BinaryAnalysisResult s PPC.Instruction a 32 PPC.PPC32) PPC.PPC32 =
    PPC32Instruction (PPC.Instruction ())
  data Operand (BinaryAnalysisResult s PPC.Instruction a 32 PPC.PPC32) PPC.PPC32 =
    PPC32Operand (Some DPPC.Operand)

instance Architecture (BinaryAnalysisResult s PPC.Instruction a 64 PPC.PPC64) PPC.PPC64 where
  data AnalysisResult (BinaryAnalysisResult s PPC.Instruction a 64 PPC.PPC64) PPC.PPC64 s =
    PPC64AnalysisResult (BinaryAnalysisResult s PPC.Instruction a 64 PPC.PPC64)
  data Block (BinaryAnalysisResult s PPC.Instruction a 64 PPC.PPC64) PPC.PPC64 =
    PPC64Block (R.ConcreteBlock PPC.Instruction 64)
  data Instruction (BinaryAnalysisResult s PPC.Instruction a 64 PPC.PPC64) PPC.PPC64 =
    PPC64Instruction (PPC.Instruction ())
  data Operand (BinaryAnalysisResult s PPC.Instruction a 64 PPC.PPC64) PPC.PPC64 =
    PPC64Operand (Some DPPC.Operand)

instance Architecture (BinaryAnalysisResult s X86.Instruction a 64 X86.X86_64) X86.X86_64 where
  data AnalysisResult (BinaryAnalysisResult s X86.Instruction a 64 X86.X86_64) X86.X86_64 s =
    X86AnalysisResult (BinaryAnalysisResult s X86.Instruction a 64 X86.X86_64)
  data Block (BinaryAnalysisResult s X86.Instruction a 64 X86.X86_64) X86.X86_64 =
    X86Block (R.ConcreteBlock X86.Instruction 64)
  data Instruction (BinaryAnalysisResult s X86.Instruction a 64 X86.X86_64) X86.X86_64 =
    X86Instruction (X86.Instruction ())
  data Operand (BinaryAnalysisResult s X86.Instruction a 64 X86.X86_64) X86.X86_64 =
    X86Operand FD.Value FD.OperandType
