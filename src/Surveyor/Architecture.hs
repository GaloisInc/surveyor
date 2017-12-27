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
import           Data.Word ( Word32, Word64 )
import           Data.Void
import           Text.Read ( readMaybe )

import qualified Data.Macaw.Memory as MM
import qualified Dismantle.PPC as DPPC
import qualified Flexdis86 as FD
import qualified Renovate as R
import qualified Renovate.Arch.PPC as PPC
import qualified Renovate.Arch.X86_64 as X86

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

mkPPC32Result :: BinaryAnalysisResult s PPC.Instruction (PPC.TargetAddress 32) 32 PPC.PPC32
              -> SomeResult s
mkPPC32Result = SomeResult . PPC32AnalysisResult

mkPPC64Result :: BinaryAnalysisResult s PPC.Instruction (PPC.TargetAddress 64) 64 PPC.PPC64
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

mcParseAddress32 :: String -> Maybe (MM.MemAddr 32)
mcParseAddress32 t = (MM.absoluteAddr . fromIntegral) <$> mn
  where
    mn :: Maybe Word32
    mn = readMaybe t

mcParseAddress64 :: String -> Maybe (MM.MemAddr 64)
mcParseAddress64 t = (MM.absoluteAddr . fromIntegral) <$> mn
  where
    mn :: Maybe Word64
    mn = readMaybe t

instance Architecture Void s where
  data AnalysisResult Void s = VoidAnalysisResult Void
  data Block Void s = VoidBlock Void
  data Instruction Void s = VoidInstruction Void
  data Operand Void s = VoidOperand Void
  data Address Void s = VoidAddress Void
  summarizeResult (VoidAnalysisResult v) = absurd v
  archNonce (VoidAnalysisResult v) = absurd v
  parseAddress _ = Nothing

instance Architecture PPC.PPC32 s where
  data AnalysisResult PPC.PPC32 s =
    PPC32AnalysisResult (BinaryAnalysisResult s PPC.Instruction (PPC.TargetAddress 32) 32 PPC.PPC32)
  data Block PPC.PPC32 s = PPC32Block (R.ConcreteBlock PPC.Instruction 32)
  data Instruction PPC.PPC32 s = PPC32Instruction (PPC.Instruction ())
  data Operand PPC.PPC32 s = PPC32Operand (Some DPPC.Operand)
  data Address PPC.PPC32 s = PPC32Address (MM.MemAddr 32)

  summarizeResult (PPC32AnalysisResult bar) = mcSummarize bar
  archNonce (PPC32AnalysisResult bar) = mcNonce bar
  parseAddress t = PPC32Address <$> mcParseAddress32 t

instance Architecture PPC.PPC64 s where
  data AnalysisResult PPC.PPC64 s =
    PPC64AnalysisResult (BinaryAnalysisResult s PPC.Instruction (PPC.TargetAddress 64) 64 PPC.PPC64)
  data Block PPC.PPC64 s = PPC64Block (R.ConcreteBlock PPC.Instruction 64)
  data Instruction PPC.PPC64 s = PPC64Instruction (PPC.Instruction ())
  data Operand PPC.PPC64 s = PPC64Operand (Some DPPC.Operand)
  data Address PPC.PPC64 s = PPC64Address (MM.MemAddr 64)

  summarizeResult (PPC64AnalysisResult bar) = mcSummarize bar
  archNonce (PPC64AnalysisResult bar) = mcNonce bar
  parseAddress t = PPC64Address <$> mcParseAddress64 t

instance Architecture X86.X86_64 s where
  data AnalysisResult X86.X86_64 s =
    X86AnalysisResult (BinaryAnalysisResult s X86.Instruction (X86.TargetAddress 64) 64 X86.X86_64)
  data Block X86.X86_64 s = X86Block (R.ConcreteBlock X86.Instruction 64)
  data Instruction X86.X86_64 s = X86Instruction (X86.Instruction ())
  data Operand X86.X86_64 s = X86Operand FD.Value FD.OperandType
  data Address X86.X86_64 s = X86Address (MM.MemAddr 64)

  summarizeResult (X86AnalysisResult bar) = mcSummarize bar
  archNonce (X86AnalysisResult bar) = mcNonce bar
  parseAddress t = X86Address <$> mcParseAddress64 t
