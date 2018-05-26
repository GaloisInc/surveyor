{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Instantiations of the 'Architecture' class for machine code architectures
module Surveyor.Core.Architecture.MC (
  mkPPC32Result,
  mkPPC64Result,
  mkX86Result
  ) where

import qualified Data.Functor.Const as C
import           Data.Int ( Int16, Int64 )
import qualified Data.Map as M
import           Data.Monoid ( (<>) )
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
import           Numeric ( showHex )
import qualified Text.PrettyPrint.HughesPJClass as HPJ
import           Text.Read ( readMaybe )

import qualified Data.Macaw.Discovery as MD
import qualified Data.Macaw.CFG as MM
import qualified Dismantle.PPC as DPPC
import qualified Flexdis86 as FD
import qualified Renovate as R
import qualified Renovate.Arch.PPC as PPC
import qualified Renovate.Arch.X86_64 as X86
import qualified Data.Parameterized.HasRepr as HR

import           Surveyor.Core.Architecture.Class
import           Surveyor.Core.BinaryAnalysisResult

mkPPC32Result :: BinaryAnalysisResult s (DPPC.Opcode DPPC.Operand) PPC.PPC32
              -> SomeResult s PPC.PPC32
mkPPC32Result = SomeResult . PPC32AnalysisResult

mkPPC64Result :: BinaryAnalysisResult s (DPPC.Opcode DPPC.Operand) PPC.PPC64
              -> SomeResult s PPC.PPC64
mkPPC64Result = SomeResult . PPC64AnalysisResult

mkX86Result :: BinaryAnalysisResult s (C.Const Void) X86.X86_64
            -> SomeResult s X86.X86_64
mkX86Result = SomeResult . X86AnalysisResult

-- TODO
--
-- * Overlapping functions
-- * Blocks ending with unknowns
-- * Functions with blocks ending in unknowns
-- * Unrecognized instructions
mcSummarize :: BinaryAnalysisResult s o arch -> [(T.Text, T.Text)]
mcSummarize bar =
  [ ("Discovered Functions", T.pack (show (length (R.biFunctionEntries binfo))))
  , ("Discovered Blocks", T.pack (show (length (R.biBlocks binfo))))
  ]
  where
    binfo = rBlockInfo bar

mcNonce :: BinaryAnalysisResult s o arch -> NG.Nonce s arch
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

mcContainingBlocks :: (R.ConcreteBlock arch -> Block arch s)
                   -> BinaryAnalysisResult s o arch
                   -> MM.MemAddr (MM.ArchAddrWidth arch)
                   -> [Block arch s]
mcContainingBlocks toBlock bar addr = map toBlock (blocksContaining bar addr)

mcFunctions :: (w ~ MM.ArchAddrWidth arch, MM.MemWidth w)
            => (MM.MemAddr w -> Address arch s)
            -> BinaryAnalysisResult s o arch
            -> [FunctionHandle arch s]
mcFunctions toAddr bar =
  [ FunctionHandle (toAddr (MM.absoluteAddr (R.absoluteAddress addr))) textName
  | (addr, Some dfi) <- M.toList (R.biDiscoveryFunInfo (rBlockInfo bar))
  , let textName = TE.decodeUtf8With TE.lenientDecode (MD.discoveredFunName dfi)
  ]

mcFunctionBlocks :: (MM.MemWidth (MM.ArchAddrWidth arch))
                 => (R.ConcreteBlock arch -> b)
                 -> BinaryAnalysisResult s o arch
                 -> R.ConcreteAddress arch
                 -> [b]
mcFunctionBlocks toBlock bar concAddr =
  case M.lookup concAddr fb of
    Nothing -> []
    Just cbs -> map toBlock cbs
  where
    bi = rBlockInfo bar
    fb = R.biFunctionBlocks bi

toInstPPC32 :: (PPC.Instruction (), R.ConcreteAddress PPC.PPC32) -> (Address PPC.PPC32 s, Instruction PPC.PPC32 s)
toInstPPC32 (i, addr) = (PPC32Address (MM.absoluteAddr (R.absoluteAddress addr)),
                         PPC32Instruction i)

toBlockPPC32 :: BinaryAnalysisResult s (DPPC.Opcode DPPC.Operand) PPC.PPC32 -> R.ConcreteBlock PPC.PPC32 -> Block PPC.PPC32 s
toBlockPPC32 bar cb =
  Block { blockAddress = PPC32Address (MM.absoluteAddr (R.absoluteAddress (R.basicBlockAddress cb)))
        , blockInstructions = map toInstPPC32 (R.instructionAddresses (rISA bar) cb)
        }

toInstPPC64 :: (PPC.Instruction (), R.ConcreteAddress PPC.PPC64) -> (Address PPC.PPC64 s, Instruction PPC.PPC64 s)
toInstPPC64 (i, addr) = (PPC64Address (MM.absoluteAddr (R.absoluteAddress addr)),
                         PPC64Instruction i)

toBlockPPC64 :: BinaryAnalysisResult s (DPPC.Opcode DPPC.Operand) PPC.PPC64 -> R.ConcreteBlock PPC.PPC64 -> Block PPC.PPC64 s
toBlockPPC64 bar cb =
  Block { blockAddress = PPC64Address (MM.absoluteAddr (R.absoluteAddress (R.basicBlockAddress cb)))
        , blockInstructions = map toInstPPC64 (R.instructionAddresses (rISA bar) cb)
        }

toBlockX86 :: BinaryAnalysisResult s (C.Const Void) X86.X86_64
           -> R.ConcreteBlock X86.X86_64
           -> Block X86.X86_64 s
toBlockX86 bar cb =
  Block { blockAddress = X86Address (MM.absoluteAddr (R.absoluteAddress (R.basicBlockAddress cb)))
        , blockInstructions = map toInstX86 (R.instructionAddresses (rISA bar) cb)
        }

toInstX86 :: (X86.Instruction (), R.ConcreteAddress X86.X86_64) -> (Address X86.X86_64 s, Instruction X86.X86_64 s)
toInstX86 (i, addr) = (X86Address (MM.absoluteAddr (R.absoluteAddress addr)),
                       X86Instruction i)

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
  prettyInstruction _ (VoidInstruction v) = absurd v
  opcode (VoidInstruction v) = absurd v
  operands (VoidInstruction v) = absurd v
  boundValue (VoidInstruction v) = absurd v
  prettyOperand _ (VoidOperand v) = absurd v
  prettyOpcode (VoidOpcode v) = absurd v
  genericSemantics (VoidAnalysisResult v) _ = absurd v
  functionBlocks (VoidAnalysisResult v) _ = absurd v

instance Eq (Address Void s) where
  VoidAddress v == _ = absurd v

instance Ord (Address Void s) where
  compare (VoidAddress v) _ = absurd v

instance Show (Address Void s) where
  show (VoidAddress v) = absurd v

instance Architecture PPC.PPC32 s where
  data AnalysisResult PPC.PPC32 s =
    PPC32AnalysisResult !(BinaryAnalysisResult s (DPPC.Opcode DPPC.Operand) PPC.PPC32)
  data Instruction PPC.PPC32 s = PPC32Instruction !(PPC.Instruction ())
  data Operand PPC.PPC32 s = forall x . PPC32Operand !(DPPC.Operand x)
  data Opcode PPC.PPC32 s = forall x y . PPC32Opcode !(DPPC.Opcode x y)
  data Address PPC.PPC32 s = PPC32Address !(MM.MemAddr 32)

  summarizeResult (PPC32AnalysisResult bar) = mcSummarize bar
  archNonce (PPC32AnalysisResult bar) = mcNonce bar
  parseAddress t = PPC32Address <$> mcParseAddress32 t
  prettyAddress (PPC32Address addr) = mcPrettyAddress addr
  prettyInstruction _ (PPC32Instruction i) = mcPrettyInstruction i
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
  functionBlocks (PPC32AnalysisResult ares) fh =
    let PPC32Address addr0 = fhAddress fh
        addr1 = MM.addrOffset addr0
        addr2 = R.concreteFromAbsolute addr1
    in mcFunctionBlocks (toBlockPPC32 ares) ares addr2

instance Eq (Address PPC.PPC32 s) where
  PPC32Address a1 == PPC32Address a2 = a1 == a2

instance Ord (Address PPC.PPC32 s) where
  PPC32Address a1 `compare` PPC32Address a2 = a1 `compare` a2

instance Show (Address PPC.PPC32 s) where
  show (PPC32Address a) = show a

instance Architecture PPC.PPC64 s where
  data AnalysisResult PPC.PPC64 s =
    PPC64AnalysisResult !(BinaryAnalysisResult s (DPPC.Opcode DPPC.Operand) PPC.PPC64)
  data Instruction PPC.PPC64 s = PPC64Instruction !(PPC.Instruction ())
  data Operand PPC.PPC64 s = forall x . PPC64Operand !(DPPC.Operand x)
  data Opcode PPC.PPC64 s = forall x y . PPC64Opcode !(DPPC.Opcode x y)
  data Address PPC.PPC64 s = PPC64Address !(MM.MemAddr 64)

  summarizeResult (PPC64AnalysisResult bar) = mcSummarize bar
  archNonce (PPC64AnalysisResult bar) = mcNonce bar
  parseAddress t = PPC64Address <$> mcParseAddress64 t
  prettyAddress (PPC64Address addr) = mcPrettyAddress addr
  prettyInstruction _ (PPC64Instruction i) = mcPrettyInstruction i
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
  functionBlocks (PPC64AnalysisResult ares) fh =
    let PPC64Address addr0 = fhAddress fh
        addr1 = MM.addrOffset addr0
        addr2 = R.concreteFromAbsolute addr1
    in mcFunctionBlocks (toBlockPPC64 ares) ares addr2

instance Eq (Address PPC.PPC64 s) where
  PPC64Address a1 == PPC64Address a2 = a1 == a2

instance Ord (Address PPC.PPC64 s) where
  PPC64Address a1 `compare` PPC64Address a2 = a1 `compare` a2

instance Show (Address PPC.PPC64 s) where
  show (PPC64Address a) = show a

instance Architecture X86.X86_64 s where
  data AnalysisResult X86.X86_64 s =
    X86AnalysisResult (BinaryAnalysisResult s (C.Const Void) X86.X86_64)
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
  functions (X86AnalysisResult bar) = mcFunctions X86Address bar
  prettyOpcode (X86Opcode s) = T.pack s
  prettyOperand (X86Address addr) (X86Operand v _) =
    let waddr = fromIntegral (MM.addrOffset addr)
    in T.pack (show (ppValue waddr v))
  prettyInstruction (X86Address addr) (X86Instruction i) =
    T.pack (show (FD.ppInstruction (fromIntegral (MM.addrOffset addr)) (X86.toFlexInst i)))
  containingBlocks (X86AnalysisResult bar) (X86Address addr) =
    mcContainingBlocks (toBlockX86 bar) bar addr
  functionBlocks (X86AnalysisResult ares) fh =
    let X86Address addr0 = fhAddress fh
        addr1 = MM.addrOffset addr0
        addr2 = R.concreteFromAbsolute addr1
    in mcFunctionBlocks (toBlockX86 ares) ares addr2


instance Eq (Address X86.X86_64 s) where
  X86Address a1 == X86Address a2 = a1 == a2

instance Ord (Address X86.X86_64 s) where
  X86Address a1 `compare` X86Address a2 = a1 `compare` a2

instance Show (Address X86.X86_64 s) where
  show (X86Address a) = show a

-- This whole bit is unfortunate - we can probably export ppValue from flexdis
ppShowReg :: Show r => r -> HPJ.Doc
ppShowReg r = HPJ.text (show r)

ppValue :: Word64 -- ^ Base address for offset printing.
                  -- This should be the address of the next instruction.
        -> FD.Value
        -> HPJ.Doc
ppValue base v =
  case v of
    FD.ControlReg   r    -> HPJ.text (show r)
    FD.DebugReg     r    -> HPJ.text (show r)
    FD.MMXReg       r    -> HPJ.text (show r)
    FD.XMMReg       r    -> HPJ.text (show r)
    FD.YMMReg       r    -> HPJ.text (show r)
    FD.X87Register  n    -> HPJ.text "st" <> if n == 0 then HPJ.empty else HPJ.parens (HPJ.int n)
    FD.SegmentValue r    -> ppShowReg    r
    -- do the "*" belong here or in ppAddrRef?
    FD.FarPointer   addr -> HPJ.text "??FAR PTR??"            HPJ.<+> ppAddrRef    addr
    FD.VoidMem      addr -> ppAddrRef addr
    FD.Mem8         addr -> HPJ.text "BYTE PTR"    HPJ.<+> ppAddrRef addr
    FD.Mem16        addr -> HPJ.text "WORD PTR"    HPJ.<+> ppAddrRef addr
    FD.Mem32        addr -> HPJ.text "DWORD PTR"   HPJ.<+> ppAddrRef addr
    FD.Mem64        addr -> HPJ.text "QWORD PTR"   HPJ.<+> ppAddrRef addr
    FD.Mem128       addr -> HPJ.text "XMMWORD PTR" HPJ.<+> ppAddrRef addr
    FD.Mem256       addr -> HPJ.text "YMMWORD PTR" HPJ.<+> ppAddrRef addr
    FD.FPMem32      addr -> HPJ.text "DWORD PTR"   HPJ.<+> ppAddrRef addr
    FD.FPMem64      addr -> HPJ.text "QWORD PTR"   HPJ.<+> ppAddrRef addr
    FD.FPMem80      addr -> HPJ.text "TBYTE PTR"   HPJ.<+> ppAddrRef addr
    FD.ByteImm      imm  -> ppImm imm
    FD.WordImm      imm  -> ppImm imm
    FD.DWordImm     (FD.Imm32Concrete imm)  -> ppImm imm
    FD.DWordImm     (FD.Imm32SymbolOffset sym off) ->
      HPJ.text (show sym) <> HPJ.text "+" <> ppImm off
    FD.QWordImm     imm  -> ppImm imm
    FD.ByteSignedImm i8 -> ppImm i8
    FD.WordSignedImm i16 -> ppImm i16
    FD.DWordSignedImm i32 -> ppImm i32
    FD.ByteReg      r    -> ppShowReg    r
    FD.WordReg      r    -> ppShowReg    r
    FD.DWordReg     r    -> ppShowReg    r
    FD.QWordReg     r    -> ppShowReg    r
    FD.JumpOffset _ (FD.FixedOffset off)  -> HPJ.text (showHex (base+fromIntegral off) "")
    FD.JumpOffset _ (FD.RelativeOffset sym ioff off) ->
      HPJ.text (show sym) <> HPJ.text "+" <> HPJ.int (fromIntegral off - fromIntegral (fromIntegral base + ioff))


ppImm :: (Integral w, Show w) => w -> HPJ.Doc
ppImm i | i >= 0 = HPJ.text "0x" <> HPJ.text (showHex i "")
          -- Print negation after converting to integer
          -- Recall that  "negate minBound = minBound" with types like Int16, Int32, Int64.
        | otherwise = HPJ.text "-0x" <> HPJ.text (showHex (negate (toInteger i)) "")

ppAddrRef :: FD.AddrRef -> HPJ.Doc
ppAddrRef addr =
  case addr of
    FD.Addr_32 seg base roff off ->
       case base of
         Just r | FD.isDefaultSeg32 seg r -> a
                | seg == FD.FS -> HPJ.text (show seg) <> (HPJ.colon HPJ.<+> a)
                | seg == FD.GS -> HPJ.text (show seg) <> (HPJ.colon HPJ.<+> a)
                | otherwise -> a -- ((HPJ.text (show seg) <> colon) <+>)
         _ -> a
      where a = ppAddr base roff off
                                                          -- or rip? this is 32 bits ...
    FD.IP_Offset_32 _seg off     -> HPJ.brackets $ HPJ.text "ip" <> appendDisplacement off
    FD.Offset_32 seg off         -> prefix seg off
    FD.Offset_64 seg off         -> prefix seg off
    FD.Addr_64 seg base roff off
        | seg == FD.FS || seg == FD.GS -> HPJ.text (show seg) <> HPJ.colon <> a
        | isDef     -> a
        | otherwise -> a
      where a = ppAddr base roff off
            isDef = maybe False (FD.isDefaultSeg64 seg) base

    FD.IP_Offset_64 _seg off -> HPJ.brackets $ HPJ.text "rip" <> appendDisplacement off
  where
    prefix seg off = ppShowReg seg <> HPJ.colon <> HPJ.text (show off)

    ppAddr :: Show r
           => Maybe r -- Base value
           -> Maybe (Int, r) -- Relative offset
           -> FD.Displacement -- Offset
           -> HPJ.Doc
    ppAddr base roff off =
      case (base, roff) of
         (Nothing, Nothing)     -> prettyDisplacement off
         (Nothing, Just (n, r)) ->
           HPJ.brackets (HPJ.text (show r) <> HPJ.text "*" <> HPJ.int n <> appendDisplacement off)
         (Just r, Nothing)      -> HPJ.brackets $
           HPJ.text (show r) <> appendDisplacement off
         (Just r, Just (n, r')) ->
           HPJ.brackets $
             HPJ.text (show r) <> HPJ.text "+" <> HPJ.text (show r') <> HPJ.text "*" <> HPJ.int n <> appendDisplacement off

appendDisplacement :: FD.Displacement -> HPJ.Doc
appendDisplacement FD.NoDisplacement = HPJ.text ""
appendDisplacement (FD.Disp32 x)
  | x >  0    = HPJ.text ("+0x" ++ showHex x "")
  | x == 0    = HPJ.text ""
  | otherwise = HPJ.text ("-0x" ++ showHex (negate (fromIntegral x :: Int64)) "")
appendDisplacement (FD.Disp8 x)
  | x >  0    = HPJ.text ("+0x" ++ showHex x "")
  | x == 0    = HPJ.text ""
  | otherwise = HPJ.text ("-0x" ++ showHex (negate (fromIntegral x :: Int16)) "")

prettyDisplacement :: FD.Displacement -> HPJ.Doc
prettyDisplacement FD.NoDisplacement = HPJ.text "0"
prettyDisplacement (FD.Disp32 x) =
  if x >= 0 then
    HPJ.text ("0x" ++ showHex x "")
   else
    HPJ.text ("-0x" ++ showHex (negate (fromIntegral x :: Int64)) "")
prettyDisplacement (FD.Disp8 x) =
  if x >= 0 then
    HPJ.text ("0x" ++ showHex x "")
   else
    HPJ.text ("-0x" ++ showHex (negate (fromIntegral x :: Int16)) "")
