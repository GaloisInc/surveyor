{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
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
  FunctionHandle(..),
  ParameterizedFormula(..),
  prettyParameterizedFormula,
  SomeResult(..),
  mkPPC32Result,
  mkPPC64Result,
  mkX86Result
  ) where

import qualified Data.Functor.Const as C
import qualified Data.Map as M
import qualified Data.Parameterized.TraversableFC as FC
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.Nonce as NG
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Data.Text.Prettyprint.Doc as PP
import           Data.Word ( Word32, Word64 )
import           Data.Void
import qualified Text.PrettyPrint.HughesPJClass as HPJ
import           Text.Read ( readMaybe )

import qualified Data.Macaw.Discovery as MD
import qualified Data.Macaw.Memory as MM
import qualified Dismantle.PPC as DPPC
import qualified Flexdis86 as FD
import qualified Renovate as R
import qualified Renovate.Arch.PPC as PPC
import qualified Renovate.Arch.X86_64 as X86
import qualified Data.Parameterized.HasRepr as HR
import qualified SemMC.Architecture as SA
import qualified SemMC.Formula as F
import qualified Lang.Crucible.Solver.SimpleBackend as SB

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
  prettyOperand :: Address arch s -> Operand arch s -> T.Text
  prettyOpcode :: Opcode arch s -> T.Text

  functions :: AnalysisResult arch s -> [FunctionHandle arch s]
  opcode :: Instruction arch s -> Opcode arch s
  operands :: Instruction arch s -> [Operand arch s]
  boundValue :: Instruction arch s -> Maybe (Operand arch s)
  genericSemantics :: AnalysisResult arch s -> Instruction arch s -> Maybe (ParameterizedFormula arch s)

data ParameterizedFormula arch s where
  ParameterizedFormula :: (SA.Architecture arch) => SA.ShapeRepr arch tp -> F.ParameterizedFormula (SB.SimpleBackend s) arch tp -> ParameterizedFormula arch s

prettyParameterizedFormula :: ParameterizedFormula arch s -> T.Text
prettyParameterizedFormula (ParameterizedFormula repr f) = F.printFormula repr f

data Block arch s =
  Block { blockAddress :: !(Address arch s)
        , blockInstructions :: [(Address arch s, Instruction arch s)]
        }

data FunctionHandle arch s =
  FunctionHandle { fhAddress :: !(Address arch s)
                 , fhName :: T.Text
                 }

mkPPC32Result :: BinaryAnalysisResult s (DPPC.Opcode DPPC.Operand) PPC.Instruction (PPC.TargetAddress 32) 32 PPC.PPC32
              -> SomeResult s
mkPPC32Result = SomeResult . PPC32AnalysisResult

mkPPC64Result :: BinaryAnalysisResult s (DPPC.Opcode DPPC.Operand) PPC.Instruction (PPC.TargetAddress 64) 64 PPC.PPC64
              -> SomeResult s
mkPPC64Result = SomeResult . PPC64AnalysisResult

mkX86Result :: BinaryAnalysisResult s (C.Const Void) X86.Instruction (X86.TargetAddress 64) 64 X86.X86_64
            -> SomeResult s
mkX86Result = SomeResult . X86AnalysisResult

-- TODO
--
-- * Overlapping functions
-- * Blocks ending with unknowns
-- * Functions with blocks ending in unknowns
-- * Unrecognized instructions
mcSummarize :: BinaryAnalysisResult s o i a w arch -> [(T.Text, T.Text)]
mcSummarize bar =
  [ ("Discovered Functions", T.pack (show (length (R.biFunctionEntries binfo))))
  , ("Discovered Blocks", T.pack (show (length (R.biBlocks binfo))))
  ]
  where
    binfo = rBlockInfo bar

mcNonce :: BinaryAnalysisResult s o i a w arch -> NG.Nonce s arch
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
                   -> BinaryAnalysisResult s o i a w arch
                   -> MM.MemAddr w
                   -> [Block arch s]
mcContainingBlocks toBlock bar addr = map toBlock (blocksContaining bar addr)

mcFunctions :: (MM.MemWidth w)
            => (MM.MemAddr w -> Address arch s)
            -> BinaryAnalysisResult s o i a w arch
            -> [FunctionHandle arch s]
mcFunctions toAddr bar =
  [ FunctionHandle (toAddr (MM.absoluteAddr (R.absoluteAddress addr))) textName
  | (addr, Some dfi) <- M.toList (R.biDiscoveryFunInfo (rBlockInfo bar))
  , let textName = TE.decodeUtf8With TE.lenientDecode (MD.discoveredFunName dfi)
  ]

toInstPPC32 :: (PPC.Instruction (), R.ConcreteAddress 32) -> (Address PPC.PPC32 s, Instruction PPC.PPC32 s)
toInstPPC32 (i, addr) = (PPC32Address (MM.absoluteAddr (R.absoluteAddress addr)),
                         PPC32Instruction i)

toBlockPPC32 :: BinaryAnalysisResult s (DPPC.Opcode DPPC.Operand) PPC.Instruction a 32 PPC.PPC32 -> R.ConcreteBlock PPC.Instruction 32 -> Block PPC.PPC32 s
toBlockPPC32 bar cb =
  Block { blockAddress = PPC32Address (MM.absoluteAddr (R.absoluteAddress (R.basicBlockAddress cb)))
        , blockInstructions = map toInstPPC32 (R.instructionAddresses (rISA bar) (rMemory bar) cb)
        }

toInstPPC64 :: (PPC.Instruction (), R.ConcreteAddress 64) -> (Address PPC.PPC64 s, Instruction PPC.PPC64 s)
toInstPPC64 (i, addr) = (PPC64Address (MM.absoluteAddr (R.absoluteAddress addr)),
                         PPC64Instruction i)

toBlockPPC64 :: BinaryAnalysisResult s (DPPC.Opcode DPPC.Operand) PPC.Instruction a 64 PPC.PPC64 -> R.ConcreteBlock PPC.Instruction 64 -> Block PPC.PPC64 s
toBlockPPC64 bar cb =
  Block { blockAddress = PPC64Address (MM.absoluteAddr (R.absoluteAddress (R.basicBlockAddress cb)))
        , blockInstructions = map toInstPPC64 (R.instructionAddresses (rISA bar) (rMemory bar) cb)
        }

ppcPrettyOperand :: (MM.MemWidth w) => MM.MemAddr w -> DPPC.Operand tp -> T.Text
ppcPrettyOperand _addr op =
  case op of
    DPPC.Abscondbrtarget off -> T.pack (show (HPJ.pPrint off))
    DPPC.Absdirectbrtarget off -> T.pack (show (HPJ.pPrint off))
    DPPC.Condbrtarget off -> T.pack (show (HPJ.pPrint off))
    DPPC.Directbrtarget off -> T.pack (show (HPJ.pPrint off))
    DPPC.Calltarget off -> T.pack (show (HPJ.pPrint off))
    DPPC.Abscalltarget off -> T.pack (show (HPJ.pPrint off))
    DPPC.Crbitm cr -> T.pack (show (HPJ.pPrint cr))
    DPPC.Crbitrc cr -> T.pack (show (HPJ.pPrint cr))
    DPPC.Crrc cr -> T.pack (show (HPJ.pPrint cr))
    DPPC.Fprc fp -> T.pack (show (HPJ.pPrint fp))
    DPPC.Gprc gp -> T.pack (show (HPJ.pPrint gp))
    DPPC.Gprc_nor0 gp -> T.pack (show (HPJ.pPrint gp))
    DPPC.I1imm i -> T.pack (show (HPJ.pPrint i))
    DPPC.I32imm i -> T.pack (show (HPJ.pPrint i))
    DPPC.S16imm i -> T.pack (show (HPJ.pPrint i))
    DPPC.S16imm64 i -> T.pack (show (HPJ.pPrint i))
    DPPC.S17imm i -> T.pack (show (HPJ.pPrint i))
    DPPC.S17imm64 i -> T.pack (show (HPJ.pPrint i))
    DPPC.S5imm i -> T.pack (show (HPJ.pPrint i))
    DPPC.U1imm i -> T.pack (show (HPJ.pPrint i))
    DPPC.U2imm i -> T.pack (show (HPJ.pPrint i))
    DPPC.U4imm i -> T.pack (show (HPJ.pPrint i))
    DPPC.U5imm i -> T.pack (show (HPJ.pPrint i))
    DPPC.U6imm i -> T.pack (show (HPJ.pPrint i))
    DPPC.U7imm i -> T.pack (show (HPJ.pPrint i))
    DPPC.U8imm i -> T.pack (show (HPJ.pPrint i))
    DPPC.U10imm i -> T.pack (show (HPJ.pPrint i))
    DPPC.U16imm i -> T.pack (show (HPJ.pPrint i))
    DPPC.U16imm64 i -> T.pack (show (HPJ.pPrint i))
    DPPC.Memrr m -> T.pack (show (HPJ.pPrint m))
    DPPC.Memri m -> T.pack (show (HPJ.pPrint m))
    DPPC.Memrix m -> T.pack (show (HPJ.pPrint m))
    DPPC.Memrix16 m -> T.pack (show (HPJ.pPrint m))
    DPPC.Vrrc vr -> T.pack (show (HPJ.pPrint vr))
    DPPC.Vsrc vr -> T.pack (show (HPJ.pPrint vr))

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
  opcode (VoidInstruction v) = absurd v
  operands (VoidInstruction v) = absurd v
  boundValue (VoidInstruction v) = absurd v
  prettyOperand _ (VoidOperand v) = absurd v
  prettyOpcode (VoidOpcode v) = absurd v
  genericSemantics (VoidAnalysisResult v) _ = absurd v

instance Architecture PPC.PPC32 s where
  data AnalysisResult PPC.PPC32 s =
    PPC32AnalysisResult !(BinaryAnalysisResult s (DPPC.Opcode DPPC.Operand) PPC.Instruction (PPC.TargetAddress 32) 32 PPC.PPC32)
  data Instruction PPC.PPC32 s = PPC32Instruction !(PPC.Instruction ())
  data Operand PPC.PPC32 s = forall x . PPC32Operand !(DPPC.Operand x)
  data Opcode PPC.PPC32 s = forall x y . PPC32Opcode !(DPPC.Opcode x y)
  data Address PPC.PPC32 s = PPC32Address !(MM.MemAddr 32)

  summarizeResult (PPC32AnalysisResult bar) = mcSummarize bar
  archNonce (PPC32AnalysisResult bar) = mcNonce bar
  parseAddress t = PPC32Address <$> mcParseAddress32 t
  prettyAddress (PPC32Address addr) = mcPrettyAddress addr
  prettyInstruction (PPC32Instruction i) = mcPrettyInstruction i
  containingBlocks (PPC32AnalysisResult bar) (PPC32Address addr) =
    mcContainingBlocks (toBlockPPC32 bar) bar addr
  functions (PPC32AnalysisResult bar) = mcFunctions PPC32Address bar
  opcode (PPC32Instruction i) =
    case PPC.toInst i of
      DPPC.Instruction opc _ -> PPC32Opcode opc
  operands (PPC32Instruction i) =
    case PPC.toInst i of
      DPPC.Instruction _ ops -> FC.toListFC PPC32Operand ops
  boundValue _ = Nothing
  prettyOperand (PPC32Address addr) (PPC32Operand op) =
    ppcPrettyOperand addr op
  prettyOpcode (PPC32Opcode opc) = T.pack (show opc)
  genericSemantics (PPC32AnalysisResult ares) (PPC32Instruction i) =
    case PPC.toInst i of
      DPPC.Instruction opc _ -> do
        formulas <- rSemantics ares
        ParameterizedFormula (HR.typeRepr opc) <$> MapF.lookup opc formulas

instance Architecture PPC.PPC64 s where
  data AnalysisResult PPC.PPC64 s =
    PPC64AnalysisResult !(BinaryAnalysisResult s (DPPC.Opcode DPPC.Operand) PPC.Instruction (PPC.TargetAddress 64) 64 PPC.PPC64)
  data Instruction PPC.PPC64 s = PPC64Instruction !(PPC.Instruction ())
  data Operand PPC.PPC64 s = forall x . PPC64Operand !(DPPC.Operand x)
  data Opcode PPC.PPC64 s = forall x y . PPC64Opcode !(DPPC.Opcode x y)
  data Address PPC.PPC64 s = PPC64Address !(MM.MemAddr 64)

  summarizeResult (PPC64AnalysisResult bar) = mcSummarize bar
  archNonce (PPC64AnalysisResult bar) = mcNonce bar
  parseAddress t = PPC64Address <$> mcParseAddress64 t
  prettyAddress (PPC64Address addr) = mcPrettyAddress addr
  prettyInstruction (PPC64Instruction i) = mcPrettyInstruction i
  containingBlocks (PPC64AnalysisResult bar) (PPC64Address addr) =
    mcContainingBlocks (toBlockPPC64 bar) bar addr
  functions (PPC64AnalysisResult bar) = mcFunctions PPC64Address bar
  opcode (PPC64Instruction i) =
    case PPC.toInst i of
      DPPC.Instruction opc _ -> PPC64Opcode opc
  operands (PPC64Instruction i) =
    case PPC.toInst i of
      DPPC.Instruction _ ops -> FC.toListFC PPC64Operand ops
  boundValue _ = Nothing
  prettyOperand (PPC64Address addr) (PPC64Operand op) =
    ppcPrettyOperand addr op
  prettyOpcode (PPC64Opcode opc) = T.pack (show opc)
  genericSemantics (PPC64AnalysisResult ares) (PPC64Instruction i) =
    case PPC.toInst i of
      DPPC.Instruction opc _ -> do
        formulas <- rSemantics ares
        ParameterizedFormula (HR.typeRepr opc) <$> MapF.lookup opc formulas

instance Architecture X86.X86_64 s where
  data AnalysisResult X86.X86_64 s =
    X86AnalysisResult (BinaryAnalysisResult s (C.Const Void) X86.Instruction (X86.TargetAddress 64) 64 X86.X86_64)
  data Instruction X86.X86_64 s = X86Instruction (X86.Instruction ())
  data Operand X86.X86_64 s = X86Operand FD.Value FD.OperandType
  data Opcode X86.X86_64 s = X86Opcode String
  data Address X86.X86_64 s = X86Address (MM.MemAddr 64)

  summarizeResult (X86AnalysisResult bar) = mcSummarize bar
  archNonce (X86AnalysisResult bar) = mcNonce bar
  parseAddress t = X86Address <$> mcParseAddress64 t
  prettyAddress (X86Address addr) = mcPrettyAddress addr
  opcode (X86Instruction i) = X86Opcode (X86.instrOpcode i)
  operands (X86Instruction i) = map (uncurry X86Operand) (X86.instrOperands i)
  boundValue _ = Nothing
  genericSemantics _ _ = Nothing
--  containingBlocks (X86AnalysisResult bar) (X86Address addr) = mcContainingBlocks bar addr
