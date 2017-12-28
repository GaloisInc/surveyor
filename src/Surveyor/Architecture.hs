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
  Block(..),
  Function(..),
  SomeResult(..),
  mkPPC32Result,
  mkPPC64Result,
  mkX86Result
  ) where

import qualified Data.Map as M
import qualified Data.Parameterized.Nonce as NG
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Data.Text.Prettyprint.Doc as PP
import           Data.Word ( Word32, Word64 )
import           Data.Void
import           Text.Read ( readMaybe )

import qualified Data.Macaw.Discovery as MD
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
  data Instruction arch s :: *
  data Operand arch s :: *
  data Opcode arch s :: *
  data Address arch s :: *

  -- | Extract the nonce for the analysis result
  archNonce :: AnalysisResult arch s -> NG.Nonce s arch
  -- | Return summary information for the artifact being analyzed; the
  -- information can vary by architecture
  summarizeResult :: AnalysisResult arch s -> [(T.Text, T.Text)]
  -- | Parse a string into an architecture-specific 'Address'
  parseAddress :: String -> Maybe (Address arch s)
  -- | Return all of the blocks that contain the given 'Address'
  containingBlocks :: AnalysisResult arch s -> Address arch s -> [Block arch s]
  -- | Pretty print an address
  prettyAddress :: Address arch s -> T.Text
  prettyInstruction :: Instruction arch s -> T.Text
  functions :: AnalysisResult arch s -> [Function arch s]

data Block arch s =
  Block { blockAddress :: !(Address arch s)
        , blockInstructions :: [(Address arch s, Instruction arch s)]
        }

data Function arch s =
  Function { functionAddress :: !(Address arch s)
           , functionName :: T.Text
           }

mkPPC32Result :: BinaryAnalysisResult s PPC.Instruction (PPC.TargetAddress 32) 32 PPC.PPC32
              -> SomeResult s
mkPPC32Result = SomeResult . PPC32AnalysisResult

mkPPC64Result :: BinaryAnalysisResult s PPC.Instruction (PPC.TargetAddress 64) 64 PPC.PPC64
              -> SomeResult s
mkPPC64Result = SomeResult . PPC64AnalysisResult

mkX86Result :: BinaryAnalysisResult s X86.Instruction (X86.TargetAddress 64) 64 X86.X86_64
            -> SomeResult s
mkX86Result = SomeResult . X86AnalysisResult

-- TODO
--
-- * Overlapping functions
-- * Blocks ending with unknowns
-- * Functions with blocks ending in unknowns
-- * Unrecognized instructions
mcSummarize :: BinaryAnalysisResult s i a w arch -> [(T.Text, T.Text)]
mcSummarize bar =
  [ ("Discovered Functions", T.pack (show (length (R.biFunctionEntries binfo))))
  , ("Discovered Blocks", T.pack (show (length (R.biBlocks binfo))))
  ]
  where
    binfo = rBlockInfo bar

mcNonce :: BinaryAnalysisResult s i a w arch -> NG.Nonce s arch
mcNonce = rNonce

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

mcPrettyAddress :: (MM.MemWidth w) => MM.MemAddr w -> T.Text
mcPrettyAddress = T.pack . show

mcPrettyInstruction :: (PP.Pretty (i ())) => i () -> T.Text
mcPrettyInstruction = T.pack . show . PP.pretty

mcContainingBlocks :: (R.ConcreteBlock i w -> Block arch s)
                   -> BinaryAnalysisResult s i a w arch
                   -> MM.MemAddr w
                   -> [Block arch s]
mcContainingBlocks toBlock bar addr = map toBlock (blocksContaining bar addr)

mcFunctions :: (MM.MemWidth w)
            => (MM.MemAddr w -> Address arch s)
            -> BinaryAnalysisResult s i a w arch
            -> [Function arch s]
mcFunctions toAddr bar =
  [ Function (toAddr (MM.absoluteAddr (R.absoluteAddress addr))) textName
  | (addr, Some dfi) <- M.toList (R.biDiscoveryFunInfo (rBlockInfo bar))
  , let textName = TE.decodeUtf8With TE.lenientDecode (MD.discoveredFunName dfi)
  ]

toInstPPC32 :: (PPC.Instruction (), R.ConcreteAddress 32) -> (Address PPC.PPC32 s, Instruction PPC.PPC32 s)
toInstPPC32 (i, addr) = (PPC32Address (MM.absoluteAddr (R.absoluteAddress addr)),
                         PPC32Instruction i)

toBlockPPC32 :: BinaryAnalysisResult s PPC.Instruction a 32 PPC.PPC32 -> R.ConcreteBlock PPC.Instruction 32 -> Block PPC.PPC32 s
toBlockPPC32 bar cb =
  Block { blockAddress = PPC32Address (MM.absoluteAddr (R.absoluteAddress (R.basicBlockAddress cb)))
        , blockInstructions = map toInstPPC32 (R.instructionAddresses (rISA bar) (rMemory bar) cb)
        }

toInstPPC64 :: (PPC.Instruction (), R.ConcreteAddress 64) -> (Address PPC.PPC64 s, Instruction PPC.PPC64 s)
toInstPPC64 (i, addr) = (PPC64Address (MM.absoluteAddr (R.absoluteAddress addr)),
                         PPC64Instruction i)

toBlockPPC64 :: BinaryAnalysisResult s PPC.Instruction a 64 PPC.PPC64 -> R.ConcreteBlock PPC.Instruction 64 -> Block PPC.PPC64 s
toBlockPPC64 bar cb =
  Block { blockAddress = PPC64Address (MM.absoluteAddr (R.absoluteAddress (R.basicBlockAddress cb)))
        , blockInstructions = map toInstPPC64 (R.instructionAddresses (rISA bar) (rMemory bar) cb)
        }

instance Architecture Void s where
  data AnalysisResult Void s = VoidAnalysisResult Void
  data Instruction Void s = VoidInstruction Void
  data Operand Void s = VoidOperand Void
  data Opcode Void s = VoidOpcode Void
  data Address Void s = VoidAddress Void
  summarizeResult (VoidAnalysisResult v) = absurd v
  archNonce (VoidAnalysisResult v) = absurd v
  parseAddress _ = Nothing
  prettyAddress (VoidAddress v) = absurd v
  containingBlocks (VoidAnalysisResult v) _ = absurd v
  functions (VoidAnalysisResult v) = absurd v
  prettyInstruction (VoidInstruction v) = absurd v

data Some2 x where
  Some2 :: !(x a b) -> Some2 x

instance Architecture PPC.PPC32 s where
  data AnalysisResult PPC.PPC32 s =
    PPC32AnalysisResult !(BinaryAnalysisResult s PPC.Instruction (PPC.TargetAddress 32) 32 PPC.PPC32)
  data Instruction PPC.PPC32 s = PPC32Instruction !(PPC.Instruction ())
  data Operand PPC.PPC32 s = PPC32Operand !(Some DPPC.Operand)
  data Opcode PPC.PPC32 s = PPC32Opcode !(Some2 DPPC.Opcode)
  data Address PPC.PPC32 s = PPC32Address !(MM.MemAddr 32)

  summarizeResult (PPC32AnalysisResult bar) = mcSummarize bar
  archNonce (PPC32AnalysisResult bar) = mcNonce bar
  parseAddress t = PPC32Address <$> mcParseAddress32 t
  prettyAddress (PPC32Address addr) = mcPrettyAddress addr
  prettyInstruction (PPC32Instruction i) = mcPrettyInstruction i
  containingBlocks (PPC32AnalysisResult bar) (PPC32Address addr) =
    mcContainingBlocks (toBlockPPC32 bar) bar addr
  functions (PPC32AnalysisResult bar) = mcFunctions PPC32Address bar

instance Architecture PPC.PPC64 s where
  data AnalysisResult PPC.PPC64 s =
    PPC64AnalysisResult !(BinaryAnalysisResult s PPC.Instruction (PPC.TargetAddress 64) 64 PPC.PPC64)
  data Instruction PPC.PPC64 s = PPC64Instruction !(PPC.Instruction ())
  data Operand PPC.PPC64 s = PPC64Operand !(Some DPPC.Operand)
  data Opcode PPC.PPC64 s = PPC64Opcode !(Some2 DPPC.Opcode)
  data Address PPC.PPC64 s = PPC64Address !(MM.MemAddr 64)

  summarizeResult (PPC64AnalysisResult bar) = mcSummarize bar
  archNonce (PPC64AnalysisResult bar) = mcNonce bar
  parseAddress t = PPC64Address <$> mcParseAddress64 t
  prettyAddress (PPC64Address addr) = mcPrettyAddress addr
  prettyInstruction (PPC64Instruction i) = mcPrettyInstruction i
  containingBlocks (PPC64AnalysisResult bar) (PPC64Address addr) =
    mcContainingBlocks (toBlockPPC64 bar) bar addr
  functions (PPC64AnalysisResult bar) = mcFunctions PPC64Address bar

instance Architecture X86.X86_64 s where
  data AnalysisResult X86.X86_64 s =
    X86AnalysisResult (BinaryAnalysisResult s X86.Instruction (X86.TargetAddress 64) 64 X86.X86_64)
  data Instruction X86.X86_64 s = X86Instruction (X86.Instruction ())
  data Operand X86.X86_64 s = X86Operand FD.Value FD.OperandType
  data Opcode X86.X86_64 s = X86Opcode String
  data Address X86.X86_64 s = X86Address (MM.MemAddr 64)

  summarizeResult (X86AnalysisResult bar) = mcSummarize bar
  archNonce (X86AnalysisResult bar) = mcNonce bar
  parseAddress t = X86Address <$> mcParseAddress64 t
  prettyAddress (X86Address addr) = mcPrettyAddress addr
--  containingBlocks (X86AnalysisResult bar) (X86Address addr) = mcContainingBlocks bar addr
