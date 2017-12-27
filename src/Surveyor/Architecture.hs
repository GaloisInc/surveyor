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

import qualified Data.Parameterized.Nonce as NG
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Text as T
import           Data.Void

import qualified Dismantle.PPC as DPPC
import qualified Flexdis86 as FD
import qualified Renovate as R
import qualified Renovate.Arch.PPC as PPC
import qualified Renovate.Arch.X86_64 as X86
import qualified SemMC.Architecture.PPC32 as PPC32
import qualified SemMC.Architecture.PPC64 as PPC64

import           Surveyor.BinaryAnalysisResult

data SomeResult s where
  SomeResult :: (Architecture arch s) => AnalysisResult arch s -> SomeResult s

class Architecture (arch :: *) (s :: *) where
  data AnalysisResult arch s :: *
  data Block arch s :: *
  data Instruction arch s :: *
  data Operand arch s :: *
  data Address arch s :: *

  archNonce :: AnalysisResult arch s -> NG.Nonce s arch
  summarizeResult :: AnalysisResult arch s -> [(T.Text, T.Text)]
  parseAddress :: String -> Maybe (Address arch s)

mkPPC32Result :: BinaryAnalysisResult s PPC.Instruction (PPC.TargetAddress 32) 32 PPC32.PPC
              -> SomeResult s
mkPPC32Result = SomeResult . PPC32AnalysisResult

mkPPC64Result :: BinaryAnalysisResult s PPC.Instruction (PPC.TargetAddress 64) 64 PPC64.PPC
              -> SomeResult s
mkPPC64Result = SomeResult . PPC64AnalysisResult

mkX86Result :: BinaryAnalysisResult s X86.Instruction (X86.TargetAddress 64) 64 X86.X86_64
            -> SomeResult s
mkX86Result = SomeResult . X86AnalysisResult

mcSummarize :: BinaryAnalysisResult s i a w arch -> [(T.Text, T.Text)]
mcSummarize bar =
  [ ("Discovered Functions", T.pack (show (length (R.biFunctionEntries binfo))))
  , ("Discovered Blocks", T.pack (show (length (R.biBlocks binfo))))
  ]
  where
    binfo = rBlockInfo bar

mcNonce :: BinaryAnalysisResult s i a w arch -> NG.Nonce s arch
mcNonce bar =
  case rNonces bar of
    (_, _, an) -> an

instance Architecture Void s where
  data AnalysisResult Void s = VoidAnalysisResult Void
  data Block Void s = VoidBlock Void
  data Instruction Void s = VoidInstruction Void
  data Operand Void s = VoidOperand Void
  data Address Void s = VoidAddress Void
  summarizeResult (VoidAnalysisResult v) = absurd v
  archNonce (VoidAnalysisResult v) = absurd v
  parseAddress _ = Nothing

instance Architecture PPC32.PPC s where
  data AnalysisResult PPC32.PPC s =
    PPC32AnalysisResult (BinaryAnalysisResult s PPC.Instruction (PPC.TargetAddress 32) 32 PPC32.PPC)
  data Block PPC32.PPC s =
    PPC32Block (R.ConcreteBlock PPC.Instruction 32)
  data Instruction PPC32.PPC s =
    PPC32Instruction (PPC.Instruction ())
  data Operand PPC32.PPC s =
    PPC32Operand (Some DPPC.Operand)

  summarizeResult (PPC32AnalysisResult bar) = mcSummarize bar
  archNonce (PPC32AnalysisResult bar) = mcNonce bar

instance Architecture PPC64.PPC s where
  data AnalysisResult PPC64.PPC s =
    PPC64AnalysisResult (BinaryAnalysisResult s PPC.Instruction (PPC.TargetAddress 64) 64 PPC64.PPC)
  data Block PPC64.PPC s =
    PPC64Block (R.ConcreteBlock PPC.Instruction 64)
  data Instruction PPC64.PPC s =
    PPC64Instruction (PPC.Instruction ())
  data Operand PPC64.PPC s =
    PPC64Operand (Some DPPC.Operand)

  summarizeResult (PPC64AnalysisResult bar) = mcSummarize bar
  archNonce (PPC64AnalysisResult bar) = mcNonce bar

instance Architecture X86.X86_64 s where
  data AnalysisResult X86.X86_64 s =
    X86AnalysisResult (BinaryAnalysisResult s X86.Instruction (X86.TargetAddress 64) 64 X86.X86_64)
  data Block X86.X86_64 s =
    X86Block (R.ConcreteBlock X86.Instruction 64)
  data Instruction X86.X86_64 s =
    X86Instruction (X86.Instruction ())
  data Operand X86.X86_64 s =
    X86Operand FD.Value FD.OperandType

  summarizeResult (X86AnalysisResult bar) = mcSummarize bar
  archNonce (X86AnalysisResult bar) = mcNonce bar
