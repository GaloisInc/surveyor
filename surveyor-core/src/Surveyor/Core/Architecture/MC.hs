{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Instantiations of the 'Architecture' class for machine code architectures
module Surveyor.Core.Architecture.MC (
  mkPPC32Result,
  mkPPC64Result,
  mkX86Result
  ) where

import           GHC.TypeNats

import           Control.DeepSeq ( NFData, rnf )
import qualified Control.Once as O
import qualified Data.Foldable as F
import qualified Data.Functor.Const as C
import           Data.Int ( Int16, Int64 )
import qualified Data.Map as M
import           Data.Parameterized.Classes ( ShowF(showF) )
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.NatRepr as NR
import qualified Data.Parameterized.Nonce as NG
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Parameterized.TraversableFC as FC
import           Data.Proxy ( Proxy(..) )
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import           Data.Void
import           Data.Word ( Word32, Word64 )
import           Numeric ( showHex )
import qualified Prettyprinter as PP
import           Text.Read ( readMaybe )

import qualified Data.Macaw.BinaryLoader as MBL
import qualified Data.Macaw.CFG as MM
import qualified Data.Macaw.Discovery as MD
import           Data.Macaw.PPC.Symbolic ()
import qualified Data.Macaw.Symbolic as MS
import qualified Data.Macaw.Types as MT
import           Data.Macaw.X86.Symbolic ()
import qualified Data.Parameterized.HasRepr as HR
import qualified Dismantle.PPC as DPPC
import qualified Flexdis86 as FD
import qualified Lang.Crucible.Backend as CB
import qualified Lang.Crucible.CFG.Core as C
import qualified Lang.Crucible.CFG.Extension as CE
import qualified Lang.Crucible.FunctionHandle as CFH
import qualified Lang.Crucible.LLVM.Intrinsics as CLLI
import qualified Lang.Crucible.LLVM.MemModel as LLM
import qualified Lang.Crucible.Simulator as CS
import qualified Lang.Crucible.Types as CT
import qualified Renovate as R
import qualified Renovate.Arch.PPC as PPC
import qualified Renovate.Arch.X86_64 as X86
import qualified What4.Interface as WI
import qualified What4.Symbol as WS

import           Surveyor.Core.Architecture.Class
import qualified Surveyor.Core.Architecture.Crucible as AC
import qualified Surveyor.Core.Architecture.Macaw as AM
import qualified Surveyor.Core.Architecture.NonceCache as SCAN
import           Surveyor.Core.BinaryAnalysisResult
import           Surveyor.Core.IRRepr ( IRRepr(MacawRepr, BaseRepr, CrucibleRepr) )
import qualified Surveyor.Core.OperandList as OL
import qualified Surveyor.Core.Panic as SCP

instance AC.CrucibleExtension PPC.PPC32 where
  type CrucibleExtensionOperand PPC.PPC32 = MacawOperand PPC.PPC32
  prettyExtensionStmt _ = prettyMacawExtensionStmt
  prettyExtensionApp _ = prettyMacawExtensionApp
  prettyExtensionOperand _ = prettyMacawExtensionOperand
  extensionExprOperands = macawExtensionExprOperands
  extensionStmtOperands = macawExtensionStmtOperands
  extensionOperandSelectable _ = macawExtensionOperandSelectable

instance AC.CrucibleExtension PPC.PPC64 where
  type CrucibleExtensionOperand PPC.PPC64 = MacawOperand PPC.PPC64
  prettyExtensionStmt _ = prettyMacawExtensionStmt
  prettyExtensionApp _ = prettyMacawExtensionApp
  prettyExtensionOperand _ = prettyMacawExtensionOperand
  extensionExprOperands = macawExtensionExprOperands
  extensionStmtOperands = macawExtensionStmtOperands
  extensionOperandSelectable _ = macawExtensionOperandSelectable

instance AC.CrucibleExtension X86.X86_64 where
  type CrucibleExtensionOperand X86.X86_64 = MacawOperand X86.X86_64
  prettyExtensionStmt _ = prettyMacawExtensionStmt
  prettyExtensionApp _ = prettyMacawExtensionApp
  prettyExtensionOperand _ = prettyMacawExtensionOperand
  extensionExprOperands = macawExtensionExprOperands
  extensionStmtOperands = macawExtensionStmtOperands
  extensionOperandSelectable _ = macawExtensionOperandSelectable

macawExtensionOperandSelectable :: MacawOperand arch s -> Bool
macawExtensionOperandSelectable o =
  case o of
    AddrRepr {} -> False
    MemRepr {} -> False
    MachineAddress {} -> True
    MacawTypeRepr {} -> False
    MachineRegister {} -> True
    NatRepr {} -> False
    MacawOverflowOp {} -> False
    Offset {} -> False
    Text {} -> False

prettyMacawExtensionStmt :: MS.MacawStmtExtension arch f tp -> PP.Doc ann
prettyMacawExtensionStmt s =
  case s of
    MS.MacawReadMem {} -> PP.pretty "macaw:read-mem"
    MS.MacawCondReadMem {} -> PP.pretty "macaw:cond-read-mem"
    MS.MacawWriteMem {} -> PP.pretty "macaw:write-mem"
    MS.MacawCondWriteMem {} -> PP.pretty "macaw:cond-write-mem"
    MS.MacawGlobalPtr {} -> PP.pretty "macaw:global-ptr"
    MS.MacawFreshSymbolic {} -> PP.pretty "macaw:fresh-symbolic"
    MS.MacawLookupFunctionHandle {} -> PP.pretty "macaw:lookup-function-handle"
    MS.MacawArchStmtExtension {} -> PP.pretty "macaw:arch-stmt-extension"
    MS.MacawArchStateUpdate {} -> PP.pretty "macaw:arch-state-update"
    MS.MacawInstructionStart {} -> PP.pretty "macaw:instruction-start"
    MS.PtrEq {} -> PP.pretty "macaw:ptr-eq"
    MS.PtrLeq {} -> PP.pretty "macaw:ptr-leq"
    MS.PtrLt {} -> PP.pretty "macaw:ptr-lt"
    MS.PtrMux {} -> PP.pretty "macaw:ptr-mux"
    MS.PtrAdd {} -> PP.pretty "macaw:ptr-add"
    MS.PtrSub {} -> PP.pretty "macaw:ptr-sub"
    MS.PtrAnd {} -> PP.pretty "macaw:ptr-and"

prettyMacawExtensionApp :: MS.MacawExprExtension arch f tp -> PP.Doc ann
prettyMacawExtensionApp e =
  case e of
    MS.MacawOverflows {} -> PP.pretty "macaw:overflows"
    MS.PtrToBits {} -> PP.pretty "macaw:ptr-to-bits"
    MS.BitsToPtr {} -> PP.pretty "macaw:bits-to-ptr"
    MS.MacawNullPtr {} -> PP.pretty "macaw:nullptr"
    MS.MacawBitcast {} -> PP.pretty "macaw:bitcast"

data MacawOperand arch s where
  AddrRepr :: MM.AddrWidthRepr (MM.ArchAddrWidth arch) -> MacawOperand arch s
  MemRepr :: MM.MemRepr tp -> MacawOperand arch s
  MachineAddress :: MM.MemAddr (MM.ArchAddrWidth arch) -> MacawOperand arch s
  MacawTypeRepr :: MT.TypeRepr tp -> MacawOperand arch s
  MachineRegister :: MM.ArchReg arch tp -> MacawOperand arch s
  NatRepr :: NR.NatRepr w -> MacawOperand arch s
  MacawOverflowOp :: MS.MacawOverflowOp -> MacawOperand arch s
  Offset :: MM.ArchAddrWord arch -> MacawOperand arch s
  Text :: T.Text -> MacawOperand arch s

prettyMacawExtensionOperand :: (ShowF (MM.ArchReg arch), MM.MemWidth (MM.ArchAddrWidth arch))
                            => MacawOperand arch s
                            -> PP.Doc ann
prettyMacawExtensionOperand o =
  case o of
    AddrRepr arep -> PP.brackets (PP.viaShow arep)
    MemRepr mrep -> PP.brackets (PP.viaShow mrep)
    MachineAddress addr -> PP.viaShow addr
    MacawTypeRepr mrep -> PP.brackets (PP.viaShow mrep)
    NatRepr mrep -> PP.brackets (PP.viaShow mrep)
    MachineRegister r -> PP.pretty (showF r)
    MacawOverflowOp oop -> PP.viaShow oop
    Text t -> PP.dquotes (PP.pretty t)
    Offset off -> PP.viaShow off

macawExtensionExprOperands :: (AC.CrucibleExtensionOperand arch ~ MacawOperand arch)
                           => SCAN.NonceCache s ctx
                           -> NG.NonceGenerator IO s
                           -> MS.MacawExprExtension arch (C.Reg ctx) tp
                           -> IO [Operand (AC.Crucible arch) s]
macawExtensionExprOperands cache ng ext =
  case ext of
    MS.PtrToBits nrep r -> do
      n <- NG.freshNonce ng
      return [ AC.toExtensionOperand n (NatRepr nrep)
             , AC.toRegisterOperand cache r
             ]
    MS.BitsToPtr nrep r -> do
      n <- NG.freshNonce ng
      return [ AC.toExtensionOperand n (NatRepr nrep)
             , AC.toRegisterOperand cache r
             ]
    MS.MacawNullPtr arep -> do
      n <- NG.freshNonce ng
      return [ AC.toExtensionOperand n (AddrRepr arep) ]
    MS.MacawOverflows oop nr r1 r2 r3 -> do
      n1 <- NG.freshNonce ng
      n2 <- NG.freshNonce ng
      return [ AC.toExtensionOperand n1 (MacawOverflowOp oop)
             , AC.toExtensionOperand n2 (NatRepr nr)
             , AC.toRegisterOperand cache r1
             , AC.toRegisterOperand cache r2
             , AC.toRegisterOperand cache r3
             ]
    MS.MacawBitcast r _proof -> do
      return [ AC.toRegisterOperand cache r ]

macawExtensionStmtOperands :: ( AC.CrucibleExtensionOperand arch ~ MacawOperand arch
                              , MM.MemWidth (MM.ArchAddrWidth arch)
                              )
                           => SCAN.NonceCache s ctx
                           -> NG.NonceGenerator IO s
                           -> MS.MacawStmtExtension arch (C.Reg ctx) tp
                           -> IO [Operand (AC.Crucible arch) s]
macawExtensionStmtOperands cache ng ext =
  case ext of
    MS.MacawReadMem arep mrep reg -> do
      n1 <- NG.freshNonce ng
      n2 <- NG.freshNonce ng
      return [ AC.toExtensionOperand n1 (AddrRepr arep)
             , AC.toExtensionOperand n2 (MemRepr mrep)
             , AC.toRegisterOperand cache reg
             ]
    MS.MacawCondReadMem arep mrep r1 r2 r3 -> do
      n1 <- NG.freshNonce ng
      n2 <- NG.freshNonce ng
      return [ AC.toExtensionOperand n1 (AddrRepr arep)
             , AC.toExtensionOperand n2 (MemRepr mrep)
             , AC.toRegisterOperand cache r1
             , AC.toRegisterOperand cache r2
             , AC.toRegisterOperand cache r3
             ]
    MS.MacawWriteMem arep mrep r1 r2 -> do
      n1 <- NG.freshNonce ng
      n2 <- NG.freshNonce ng
      return [ AC.toExtensionOperand n1 (AddrRepr arep)
             , AC.toExtensionOperand n2 (MemRepr mrep)
             , AC.toRegisterOperand cache r1
             , AC.toRegisterOperand cache r2
             ]
    MS.MacawCondWriteMem arep mrep r1 r2 r3 -> do
      n1 <- NG.freshNonce ng
      n2 <- NG.freshNonce ng
      return [ AC.toExtensionOperand n1 (AddrRepr arep)
             , AC.toExtensionOperand n2 (MemRepr mrep)
             , AC.toRegisterOperand cache r1
             , AC.toRegisterOperand cache r2
             , AC.toRegisterOperand cache r3
             ]
    MS.MacawGlobalPtr arep addr -> do
      n1 <- NG.freshNonce ng
      n2 <- NG.freshNonce ng
      return [ AC.toExtensionOperand n1 (AddrRepr arep)
             , AC.toExtensionOperand n2 (MachineAddress addr)
             ]
    MS.MacawFreshSymbolic mtp -> do
      n1 <- NG.freshNonce ng
      return [ AC.toExtensionOperand n1 (MacawTypeRepr mtp) ]
    MS.MacawLookupFunctionHandle tps r -> do
      return [ AC.toRegisterOperand cache r ]
    MS.MacawArchStmtExtension archExt -> return [] -- FIXME
    MS.MacawArchStateUpdate addr m -> do
      n1 <- NG.freshNonce ng
      let makeOpPairs (MapF.Pair machReg (MS.MacawCrucibleValue reg)) = do
            n <- NG.freshNonce ng
            return [AC.toExtensionOperand n (MachineRegister machReg), AC.toRegisterOperand cache reg]
      ops <- mapM makeOpPairs (MapF.toList m)
      return ( AC.toExtensionOperand n1 (MachineAddress addr)
             : concat ops
             )
    MS.MacawInstructionStart baddr ioff t -> do
      n1 <- NG.freshNonce ng
      n2 <- NG.freshNonce ng
      n3 <- NG.freshNonce ng
      return [ AC.toExtensionOperand n1 (MachineAddress (MM.segoffAddr baddr))
             , AC.toExtensionOperand n2 (Offset ioff)
             , AC.toExtensionOperand n3 (Text t)
             ]
    MS.PtrEq arep r1 r2 -> do
      n1 <- NG.freshNonce ng
      return [ AC.toExtensionOperand n1 (AddrRepr arep)
             , AC.toRegisterOperand cache r1
             , AC.toRegisterOperand cache r2
             ]
    MS.PtrLeq arep r1 r2 -> do
      n1 <- NG.freshNonce ng
      return [ AC.toExtensionOperand n1 (AddrRepr arep)
             , AC.toRegisterOperand cache r1
             , AC.toRegisterOperand cache r2
             ]
    MS.PtrLt arep r1 r2 -> do
      n1 <- NG.freshNonce ng
      return [ AC.toExtensionOperand n1 (AddrRepr arep)
             , AC.toRegisterOperand cache r1
             , AC.toRegisterOperand cache r2
             ]
    MS.PtrAdd arep r1 r2 -> do
      n1 <- NG.freshNonce ng
      return [ AC.toExtensionOperand n1 (AddrRepr arep)
             , AC.toRegisterOperand cache r1
             , AC.toRegisterOperand cache r2
             ]
    MS.PtrSub arep r1 r2 -> do
      n1 <- NG.freshNonce ng
      return [ AC.toExtensionOperand n1 (AddrRepr arep)
             , AC.toRegisterOperand cache r1
             , AC.toRegisterOperand cache r2
             ]
    MS.PtrAnd arep r1 r2 -> do
      n1 <- NG.freshNonce ng
      return [ AC.toExtensionOperand n1 (AddrRepr arep)
             , AC.toRegisterOperand cache r1
             , AC.toRegisterOperand cache r2
             ]
    MS.PtrMux arep r1 r2 r3 -> do
      n1 <- NG.freshNonce ng
      return [ AC.toExtensionOperand n1 (AddrRepr arep)
             , AC.toRegisterOperand cache r1
             , AC.toRegisterOperand cache r2
             , AC.toRegisterOperand cache r3
             ]

indexCrucibleMCBlocks :: ( MS.MacawStmtExtension arch ~ CE.StmtExtension (CrucibleExt arch)
                         , MM.MemWidth (MM.ArchAddrWidth arch)
                         , Ord (Address arch s)
                         )
                       => (MM.MemAddr (MM.ArchAddrWidth arch) -> Address arch s)
                      -> [(Block arch s, Block (AC.Crucible arch) s)]
                      -> M.Map (Address (AC.Crucible arch) s) (S.Set (Address arch s))
indexCrucibleMCBlocks toArchAddr blks =
  let s0 = (Nothing, M.empty)
      instrs = concatMap (blockInstructions . snd) blks
      (_, idx) = F.foldl' (buildCrucibleInstIndex toArchAddr) s0 instrs
  in idx

buildCrucibleInstIndex :: ( MS.MacawStmtExtension arch ~ CE.StmtExtension (CrucibleExt arch)
                          , MM.MemWidth (MM.ArchAddrWidth arch)
                          , Ord (Address arch s)
                          )
                       => (MM.MemAddr (MM.ArchAddrWidth arch) -> Address arch s)
                       -> (Maybe (Address arch s), M.Map (Address (AC.Crucible arch) s) (S.Set (Address arch s)))
                       -> (Address (AC.Crucible arch) s, Instruction (AC.Crucible arch) s)
                       -> (Maybe (Address arch s), M.Map (Address (AC.Crucible arch) s) (S.Set (Address arch s)))
buildCrucibleInstIndex toArchAddr acc@(mCurrentInsnAddr, m) (iaddr, i) =
  case i of
    AC.CrucibleStmt _ s _ _ ->
      case s of
        C.ExtendAssign (MS.MacawInstructionStart baddr off _txt) ->
          let cia' = toArchAddr (MM.incAddr (fromIntegral off) (MM.segoffAddr baddr))
          in (Just cia', m)
        _ | Just cia <- mCurrentInsnAddr ->
            (mCurrentInsnAddr, M.insertWith S.union iaddr (S.singleton cia) m)
          | otherwise -> acc
    AC.CrucibleTermStmt {}
      | Just cia <- mCurrentInsnAddr ->
        (mCurrentInsnAddr, M.insertWith S.union iaddr (S.singleton cia) m)
      | otherwise -> acc

mkPPC32Result :: BinaryAnalysisResult s (DPPC.Opcode DPPC.Operand) PPC.PPC32
              -> SomeResult s PPC.PPC32
mkPPC32Result bar =
  SomeResult (AnalysisResult (PPC32AnalysisResult bar) (indexBinaryAnalysisResult PPC32Address bar))

mkPPC64Result :: BinaryAnalysisResult s (DPPC.Opcode DPPC.Operand) PPC.PPC64
              -> SomeResult s PPC.PPC64
mkPPC64Result bar =
  SomeResult (AnalysisResult (PPC64AnalysisResult bar) (indexBinaryAnalysisResult PPC64Address bar))

mkX86Result :: BinaryAnalysisResult s (C.Const Void) X86.X86_64
            -> SomeResult s X86.X86_64
mkX86Result bar =
  SomeResult (AnalysisResult (X86AnalysisResult bar) (indexBinaryAnalysisResult X86Address bar))

indexBinaryAnalysisResult :: (ArchConstraints arch s, MM.MemWidth (MM.ArchAddrWidth arch))
                          => (MM.MemAddr (MM.ArchAddrWidth arch) -> Address arch s)
                          -> BinaryAnalysisResult s o arch
                          -> O.Once (ResultIndex arch s)
indexBinaryAnalysisResult addrCon bar = O.once idx
  where
    idx = ResultIndex { riFunctions = mcFunctions addrCon bar
                      , riSummary = mcSummarize bar
                      }

-- TODO
--
-- * Overlapping functions
-- * Blocks ending with unknowns
-- * Functions with blocks ending in unknowns
-- * Unrecognized instructions
mcSummarize :: BinaryAnalysisResult s o arch -> [(T.Text, T.Text)]
mcSummarize bar =
  [ (T.pack "Discovered Functions", T.pack (show (length (R.biFunctionEntries binfo))))
  , (T.pack "Discovered Blocks", T.pack (show (length (R.biBlocks binfo))))
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

mcPrettyAddress :: (MM.MemWidth w) => MM.MemAddr w -> PP.Doc ann
mcPrettyAddress = PP.viaShow

mcPrettyInstruction :: (PP.Pretty (i ())) => i () -> PP.Doc ann
mcPrettyInstruction = PP.pretty

mcContainingBlocks :: (w ~ MM.ArchAddrWidth arch, MM.MemWidth w)
                   => (MM.MemAddr w -> Address arch s)
                   -> (FunctionHandle arch s -> R.ConcreteBlock arch -> Block arch s)
                   -> BinaryAnalysisResult s o arch
                   -> MM.MemAddr (MM.ArchAddrWidth arch)
                   -> [Block arch s]
mcContainingBlocks toAddr toBlock bar addr =
  map makeBlockWithFunction (blocksContaining bar addr)
  where
    makeBlockWithFunction (b, funcs) =
      case largestFunction bar funcs of
        (funcAddr, Some largestFunc) ->
          let fh = FunctionHandle { fhAddress = toAddr (MM.absoluteAddr (R.absoluteAddress funcAddr))
                                  , fhName = TE.decodeUtf8With TE.lenientDecode (MD.discoveredFunName largestFunc)
                                  }
          in toBlock fh b


mcFunctions :: (w ~ MM.ArchAddrWidth arch, MM.MemWidth w)
            => (MM.MemAddr w -> Address arch s)
            -> BinaryAnalysisResult s o arch
            -> [FunctionHandle arch s]
mcFunctions toAddr bar =
  [ FunctionHandle (toAddr (MM.absoluteAddr (R.absoluteAddress addr))) textName
  | (addr, (_, Some dfi)) <- M.toList (R.biFunctions (rBlockInfo bar))
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
    Just (cbs, _dfi) -> map toBlock cbs
  where
    bi = rBlockInfo bar
    fb = R.biFunctions bi

toInstPPC32 :: (R.Instruction PPC.PPC32 PPC.OnlyEncoding (), R.ConcreteAddress PPC.PPC32)
            -> (Address PPC.PPC32 s, Instruction PPC.PPC32 s)
toInstPPC32 (i, addr) = (PPC32Address (MM.absoluteAddr (R.absoluteAddress addr)),
                         PPC32Instruction i)

toBlockPPC32 :: BinaryAnalysisResult s (DPPC.Opcode DPPC.Operand) PPC.PPC32 -> FunctionHandle PPC.PPC32 s -> R.ConcreteBlock PPC.PPC32 -> Block PPC.PPC32 s
toBlockPPC32 bar fh cb =
  Block { blockAddress = PPC32Address (MM.absoluteAddr (R.absoluteAddress (R.blockAddress cb)))
        , blockInstructions = R.withInstructionAddresses (rISA bar) cb $ \PPC.PPCRepr insns ->
            F.toList (fmap toInstPPC32 insns)
        , blockFunction = fh
        }

toInstPPC64 :: (R.Instruction PPC.PPC64 PPC.OnlyEncoding (), R.ConcreteAddress PPC.PPC64)
            -> (Address PPC.PPC64 s, Instruction PPC.PPC64 s)
toInstPPC64 (i, addr) = (PPC64Address (MM.absoluteAddr (R.absoluteAddress addr)),
                         PPC64Instruction i)

toBlockPPC64 :: BinaryAnalysisResult s (DPPC.Opcode DPPC.Operand) PPC.PPC64 -> FunctionHandle PPC.PPC64 s -> R.ConcreteBlock PPC.PPC64 -> Block PPC.PPC64 s
toBlockPPC64 bar fh cb =
  Block { blockAddress = PPC64Address (MM.absoluteAddr (R.absoluteAddress (R.blockAddress cb)))
        , blockInstructions = R.withInstructionAddresses (rISA bar) cb $ \PPC.PPCRepr insns ->
            F.toList (fmap toInstPPC64 insns)
        , blockFunction = fh
        }

toBlockX86 :: BinaryAnalysisResult s (C.Const Void) X86.X86_64
           -> FunctionHandle X86.X86_64 s
           -> R.ConcreteBlock X86.X86_64
           -> Block X86.X86_64 s
toBlockX86 bar fh cb =
  Block { blockAddress = X86Address (MM.absoluteAddr (R.absoluteAddress (R.blockAddress cb)))
        , blockInstructions = R.withInstructionAddresses (rISA bar) cb $ \X86.X86Repr insns ->
            F.toList (fmap toInstX86 insns)
        , blockFunction = fh
        }

toInstX86 :: (X86.Instruction X86.OnlyEncoding (), R.ConcreteAddress X86.X86_64)
          -> (Address X86.X86_64 s, Instruction X86.X86_64 s)
toInstX86 (i, addr) = (X86Address (MM.absoluteAddr (R.absoluteAddress addr)),
                       X86Instruction i)

ppcPrettyOperand :: (MM.MemWidth w) => MM.MemAddr w -> DPPC.Operand tp -> PP.Doc ann
ppcPrettyOperand _addr op =
  case op of
    DPPC.Abscondbrtarget off -> textViaShow off
    DPPC.Absdirectbrtarget off -> textViaShow off
    DPPC.Condbrtarget off -> textViaShow off
    DPPC.Directbrtarget off -> textViaShow off
    DPPC.Calltarget off -> textViaShow off
    DPPC.Abscalltarget off -> textViaShow off
    DPPC.Crbitm cr -> textViaShow cr
    DPPC.Crbitrc cr -> textViaShow cr
    DPPC.Crrc cr -> textViaShow cr
    DPPC.Fprc fp -> textViaShow fp
    DPPC.Gprc gp -> textViaShow gp
    DPPC.Gprc_nor0 gp -> textViaShow gp
    DPPC.I1imm i -> textViaShow i
    DPPC.I32imm i -> textViaShow i
    DPPC.S16imm i -> textViaShow i
    DPPC.S16imm64 i -> textViaShow i
    DPPC.S17imm i -> textViaShow i
    DPPC.S17imm64 i -> textViaShow i
    DPPC.S5imm i -> textViaShow i
    DPPC.U1imm i -> textViaShow i
    DPPC.U2imm i -> textViaShow i
    DPPC.U4imm i -> textViaShow i
    DPPC.U5imm i -> textViaShow i
    DPPC.U6imm i -> textViaShow i
    DPPC.U7imm i -> textViaShow i
    DPPC.U8imm i -> textViaShow i
    DPPC.U10imm i -> textViaShow i
    DPPC.U16imm i -> textViaShow i
    DPPC.U16imm64 i -> textViaShow i
    DPPC.Memrr m -> textViaShow m
    DPPC.Memri m -> textViaShow m
    DPPC.Memrix m -> textViaShow m
    DPPC.Memrix16 m -> textViaShow m
    DPPC.Vrrc vr -> textViaShow vr
    DPPC.Vsrc vr -> textViaShow vr

textViaShow :: (Show a) => a -> PP.Doc ann
textViaShow = PP.viaShow

instance IR PPC.PPC32 s where
  data Instruction PPC.PPC32 s = PPC32Instruction !(R.Instruction PPC.PPC32 PPC.OnlyEncoding ())
  data Operand PPC.PPC32 s = forall x . PPC32Operand !(DPPC.Operand x)
  data Opcode PPC.PPC32 s = forall x y . PPC32Opcode !(DPPC.Opcode x y)
  data Address PPC.PPC32 s = PPC32Address !(MM.MemAddr 32)

  prettyAddress (PPC32Address addr) = mcPrettyAddress addr
  prettyInstruction _ (PPC32Instruction i) = mcPrettyInstruction i
  opcode (PPC32Instruction i) =
    case R.toGenericInstruction @PPC.PPC32 i of
      DPPC.Instruction opc _ -> PPC32Opcode opc
  operands (PPC32Instruction i) =
    case R.toGenericInstruction @PPC.PPC32 i of
      DPPC.Instruction _ ops -> OL.fromList (FC.toListFC PPC32Operand ops)
  boundValue _ = Nothing
  prettyOperand (PPC32Address addr) (PPC32Operand op) =
    ppcPrettyOperand addr op
  prettyOpcode (PPC32Opcode opc) = PP.viaShow opc
  parseAddress t = PPC32Address <$> mcParseAddress32 t
  rawRepr = Just (\(PPC32Instruction i) -> PPC.assemble i)
  showInstructionAddresses _ = True
  operandSelectable (PPC32Operand o) = ppcOperandSelectable o

ppcOperandSelectable :: DPPC.Operand tp -> Bool
ppcOperandSelectable o =
  case o of
    DPPC.Gprc {} -> True
    DPPC.Gprc_nor0 {} -> True
    DPPC.Crrc {} -> True
    DPPC.Fprc {} -> True
    DPPC.Vrrc {} -> True
    DPPC.Vsrc {} -> True
    DPPC.Memrr {} -> True
    DPPC.Memri {} -> True
    DPPC.Memrix {} -> True
    DPPC.Memrix16 {} -> True

    DPPC.Abscondbrtarget {} -> True
    DPPC.Absdirectbrtarget {} -> True
    DPPC.Condbrtarget {} -> True
    DPPC.Directbrtarget {} -> True
    DPPC.Calltarget {} -> True
    DPPC.Abscalltarget {} -> True

    DPPC.Crbitm {} -> False
    DPPC.Crbitrc {} -> False
    DPPC.I1imm {} -> False
    DPPC.I32imm {} -> False
    DPPC.S16imm {} -> False
    DPPC.S16imm64 {} -> False
    DPPC.S17imm {} -> False
    DPPC.S17imm64 {} -> False
    DPPC.S5imm {} -> False
    DPPC.U1imm {} -> False
    DPPC.U2imm {} -> False
    DPPC.U4imm {} -> False
    DPPC.U5imm {} -> False
    DPPC.U6imm {} -> False
    DPPC.U7imm {} -> False
    DPPC.U8imm {} -> False
    DPPC.U10imm {} -> False
    DPPC.U16imm {} -> False
    DPPC.U16imm64 {} -> False

type instance CruciblePersonality PPC.PPC32 sym = MS.MacawSimulatorState sym

instance Architecture PPC.PPC32 s where
  data ArchResult PPC.PPC32 s =
    PPC32AnalysisResult !(BinaryAnalysisResult s (DPPC.Opcode DPPC.Operand) PPC.PPC32)
  type CrucibleExt PPC.PPC32 = MS.MacawExt PPC.PPC32

  summarizeResult (AnalysisResult _ idx) = riSummary (O.runOnce idx)
  archNonce (AnalysisResult (PPC32AnalysisResult bar) _) = mcNonce bar
  containingBlocks (AnalysisResult (PPC32AnalysisResult bar) _) (PPC32Address addr) =
    mcContainingBlocks PPC32Address (toBlockPPC32 bar) bar addr
  functions (AnalysisResult _ idx) = riFunctions (O.runOnce idx)

  genericSemantics (AnalysisResult (PPC32AnalysisResult BinaryAnalysisResult{rSemantics = sem}) _) (PPC32Instruction i) =
    case R.toGenericInstruction @PPC.PPC32 i of
      DPPC.Instruction opc _ -> do
        formulas <- sem
        ParameterizedFormula (HR.typeRepr opc) <$> MapF.lookup opc formulas
  functionBlocks (AnalysisResult (PPC32AnalysisResult ares) _) fh =
    let PPC32Address addr0 = fhAddress fh
        addr1 = MM.addrOffset addr0
        addr2 = R.concreteFromAbsolute addr1
    in mcFunctionBlocks (toBlockPPC32 ares fh) ares addr2
  alternativeIRs _ = [SomeIRRepr MacawRepr, SomeIRRepr CrucibleRepr]
  asAlternativeIR repr ar@(AnalysisResult (PPC32AnalysisResult bar@BinaryAnalysisResult {rLoadedBinary = img}) _) fh =
    case repr of
      BaseRepr -> return Nothing
      MacawRepr -> do
        let mem = MBL.memoryImage img
        let blocks = [ (segOff, b)
                     | b <- functionBlocks ar fh
                     , let PPC32Address memAddr = blockAddress b
                     , Just segOff <- [MM.asSegmentOff mem memAddr]
                     ]
        let PPC32Address memAddr = fhAddress fh
        case MM.asAbsoluteAddr memAddr of
          Just memAbsAddr -> Just <$> AM.macawForBlocks PPC32Address (rNonceGen bar) (rBlockInfo bar) (R.concreteFromAbsolute memAbsAddr) blocks
          Nothing ->
            SCP.panic "PPC32 Macaw translation" ["Invalid address for function: " ++ show memAddr
                                                ]
      CrucibleRepr -> do
        let mem = MBL.memoryImage img
        let blocks = [ (segOff, b)
                     | b <- functionBlocks ar fh
                     , let PPC32Address memAddr = blockAddress b
                     , Just segOff <- [MM.asSegmentOff mem memAddr]
                     ]
        let PPC32Address memAddr = fhAddress fh
        case MM.asAbsoluteAddr memAddr of
          Just memAbsAddr -> AC.crucibleForMCBlocks (rNonceGen bar) (indexCrucibleMCBlocks PPC32Address) (rBlockInfo bar) (R.concreteFromAbsolute memAbsAddr) blocks
          Nothing ->
            SCP.panic "PPC32 Crucible translation" ["Invalid address for function: " ++ show memAddr
                                                   ]
  crucibleCFG (AnalysisResult (PPC32AnalysisResult bar) _) = mcCrucibleCFG ppcaddr32ToConcrete bar
  freshSymbolicEntry _ = mcFreshSymbolicEntry
  symbolicInitializers = mcSymbolicInitializers
  fromCrucibleBlock = Nothing

ppcaddr32ToConcrete :: Address PPC.PPC32 s -> R.ConcreteAddress PPC.PPC32
ppcaddr32ToConcrete (PPC32Address ma32) =
  case MM.asAbsoluteAddr ma32 of
    Just absAddr -> R.concreteFromAbsolute absAddr
    Nothing ->
      SCP.panic "PPC32 renovate address translation" ["Unsupported address translation: " ++ show ma32
                                                     ]

ppcaddr64ToConcrete :: Address PPC.PPC64 s -> R.ConcreteAddress PPC.PPC64
ppcaddr64ToConcrete (PPC64Address ma64) =
  case MM.asAbsoluteAddr ma64 of
    Just absAddr -> R.concreteFromAbsolute absAddr
    Nothing ->
      SCP.panic "PPC64 renovate address translation" ["Unsupported address translation: " ++ show ma64
                                                     ]

x86addr64ToConcrete :: Address X86.X86_64 s -> R.ConcreteAddress X86.X86_64
x86addr64ToConcrete (X86Address addr) =
  case MM.asAbsoluteAddr addr of
    Just absAddr -> R.concreteFromAbsolute absAddr
    Nothing ->
      SCP.panic "X86 renovate address translation" ["Unsupported address translation: " ++ show addr
                                                   ]

mcCrucibleCFG :: (Address arch s -> R.ConcreteAddress arch)
              -> BinaryAnalysisResult s o arch
              -> FunctionHandle arch s
              -> IO (Maybe (C.AnyCFG (MS.MacawExt arch)))
mcCrucibleCFG addrToConcrete bar fh =
  case M.lookup (addrToConcrete (fhAddress fh)) cfgs of
    Nothing -> return Nothing
    Just scfg -> do
      C.SomeCFG cfg <- R.getSymbolicCFG scfg
      return (Just (C.AnyCFG cfg))
  where
    cfgs = R.biCFG (rBlockInfo bar)

instance Eq (Address PPC.PPC32 s) where
  PPC32Address a1 == PPC32Address a2 = a1 == a2

instance Ord (Address PPC.PPC32 s) where
  PPC32Address a1 `compare` PPC32Address a2 = a1 `compare` a2

instance Show (Address PPC.PPC32 s) where
  show (PPC32Address a) = show a

instance NFData (Address PPC.PPC32 s) where
  rnf (PPC32Address a) = a `seq` ()

instance NFData (Instruction PPC.PPC32 s) where
  rnf (PPC32Instruction i) = i `seq` ()

instance NFData (Operand PPC.PPC32 s) where
  rnf (PPC32Operand _) = ()

instance IR PPC.PPC64 s where
  data Instruction PPC.PPC64 s = PPC64Instruction !(R.Instruction PPC.PPC64 PPC.OnlyEncoding ())
  data Operand PPC.PPC64 s = forall x . PPC64Operand !(DPPC.Operand x)
  data Opcode PPC.PPC64 s = forall x y . PPC64Opcode !(DPPC.Opcode x y)
  data Address PPC.PPC64 s = PPC64Address !(MM.MemAddr 64)
  prettyAddress (PPC64Address addr) = mcPrettyAddress addr
  prettyInstruction _ (PPC64Instruction i) = mcPrettyInstruction i
  opcode (PPC64Instruction i) =
    case R.toGenericInstruction @PPC.PPC64 i of
      DPPC.Instruction opc _ -> PPC64Opcode opc
  operands (PPC64Instruction i) =
    case R.toGenericInstruction @PPC.PPC64 i of
      DPPC.Instruction _ ops -> OL.fromList (FC.toListFC PPC64Operand ops)
  boundValue _ = Nothing
  prettyOperand (PPC64Address addr) (PPC64Operand op) =
    ppcPrettyOperand addr op
  prettyOpcode (PPC64Opcode opc) = PP.viaShow opc
  parseAddress t = PPC64Address <$> mcParseAddress64 t
  rawRepr = Just (\(PPC64Instruction i) -> PPC.assemble i)
  showInstructionAddresses _ = True
  operandSelectable (PPC64Operand o) = ppcOperandSelectable o

type instance CruciblePersonality PPC.PPC64 sym = MS.MacawSimulatorState sym

instance Architecture PPC.PPC64 s where
  data ArchResult PPC.PPC64 s =
    PPC64AnalysisResult !(BinaryAnalysisResult s (DPPC.Opcode DPPC.Operand) PPC.PPC64)
  type CrucibleExt PPC.PPC64 = MS.MacawExt PPC.PPC64

  summarizeResult (AnalysisResult _ idx) = riSummary (O.runOnce idx)
  archNonce (AnalysisResult (PPC64AnalysisResult bar) _) = mcNonce bar
  containingBlocks (AnalysisResult (PPC64AnalysisResult bar) _) (PPC64Address addr) =
    mcContainingBlocks PPC64Address (toBlockPPC64 bar) bar addr
  functions (AnalysisResult _ idx) = riFunctions (O.runOnce idx)
  genericSemantics (AnalysisResult (PPC64AnalysisResult BinaryAnalysisResult {rSemantics = sem}) _) (PPC64Instruction i) =
    case R.toGenericInstruction @PPC.PPC64 i of
      DPPC.Instruction opc _ -> do
        formulas <- sem
        ParameterizedFormula (HR.typeRepr opc) <$> MapF.lookup opc formulas
  functionBlocks (AnalysisResult (PPC64AnalysisResult ares) _) fh =
    let PPC64Address addr0 = fhAddress fh
        addr1 = MM.addrOffset addr0
        addr2 = R.concreteFromAbsolute addr1
    in mcFunctionBlocks (toBlockPPC64 ares fh) ares addr2
  alternativeIRs _ = [SomeIRRepr MacawRepr, SomeIRRepr CrucibleRepr]
  asAlternativeIR repr ar@(AnalysisResult (PPC64AnalysisResult bar@BinaryAnalysisResult{rLoadedBinary = img}) _) fh =
    case repr of
      BaseRepr -> return Nothing
      MacawRepr -> do
        let mem = MBL.memoryImage img
        let blocks = [ (segOff, b)
                     | b <- functionBlocks ar fh
                     , let PPC64Address memAddr = blockAddress b
                     , Just segOff <- [MM.asSegmentOff mem memAddr]
                     ]
        let PPC64Address memAddr = fhAddress fh
        case MM.asAbsoluteAddr memAddr of
          Just memAbsAddr -> Just <$> AM.macawForBlocks PPC64Address (rNonceGen bar) (rBlockInfo bar) (R.concreteFromAbsolute memAbsAddr) blocks
          Nothing ->
            SCP.panic "PPC64 Macaw translation" ["Invalid address for function: " ++ show memAddr
                                                ]
      CrucibleRepr -> do
        let mem = MBL.memoryImage img
        let blocks = [ (segOff, b)
                     | b <- functionBlocks ar fh
                     , let PPC64Address memAddr = blockAddress b
                     , Just segOff <- [MM.asSegmentOff mem memAddr]
                     ]
        let PPC64Address memAddr = fhAddress fh
        case MM.asAbsoluteAddr memAddr of
          Just memAbsAddr -> AC.crucibleForMCBlocks (rNonceGen bar) (indexCrucibleMCBlocks PPC64Address) (rBlockInfo bar) (R.concreteFromAbsolute memAbsAddr) blocks
          Nothing ->
            SCP.panic "PPC64 Crucible translation" ["Invalid address for function: " ++ show memAddr
                                                   ]
  crucibleCFG (AnalysisResult (PPC64AnalysisResult bar) _) = mcCrucibleCFG ppcaddr64ToConcrete bar
  freshSymbolicEntry _ = mcFreshSymbolicEntry
  symbolicInitializers = mcSymbolicInitializers
  fromCrucibleBlock = Nothing


instance Eq (Address PPC.PPC64 s) where
  PPC64Address a1 == PPC64Address a2 = a1 == a2

instance Ord (Address PPC.PPC64 s) where
  PPC64Address a1 `compare` PPC64Address a2 = a1 `compare` a2

instance Show (Address PPC.PPC64 s) where
  show (PPC64Address a) = show a

instance NFData (Address PPC.PPC64 s) where
  rnf (PPC64Address a) = a `seq` ()

instance NFData (Instruction PPC.PPC64 s) where
  rnf (PPC64Instruction i) = i `seq` ()

instance NFData (Operand PPC.PPC64 s) where
  rnf (PPC64Operand _) = ()

instance IR X86.X86_64 s where
  data Instruction X86.X86_64 s = X86Instruction (X86.Instruction X86.OnlyEncoding ())
  data Operand X86.X86_64 s = X86Operand FD.Value FD.OperandType
  data Opcode X86.X86_64 s = X86Opcode String
  data Address X86.X86_64 s = X86Address (MM.MemAddr 64)
  prettyAddress (X86Address addr) = mcPrettyAddress addr
  opcode (X86Instruction i) = X86Opcode (X86.instrOpcode i)
  operands (X86Instruction i) = OL.fromList (map (uncurry X86Operand) (X86.instrOperands i))
  boundValue _ = Nothing
  prettyOpcode (X86Opcode s) = PP.pretty s
  prettyOperand (X86Address addr) (X86Operand v _) =
    let waddr = fromIntegral (MM.addrOffset addr)
    in PP.viaShow (ppValue waddr v)
  prettyInstruction (X86Address _addr) (X86Instruction i) =
    PP.viaShow (FD.ppInstruction (X86.toFlexInst i))
  parseAddress t = X86Address <$> mcParseAddress64 t
  rawRepr = Just (\(X86Instruction i) -> X86.assemble i)
  showInstructionAddresses _ = True
  operandSelectable (X86Operand o _) =
    case o of
      FD.ByteReg {} -> True
      FD.WordReg {} -> True
      FD.DWordReg {} -> True
      FD.QWordReg {} -> True
      FD.JumpOffset {} -> True
      FD.ControlReg {} -> True
      FD.DebugReg {} -> True
      FD.MMXReg {} -> True
      FD.XMMReg {} -> True
      FD.YMMReg {} -> True
      FD.X87Register {} -> True
      FD.FarPointer {} -> True
      FD.VoidMem {} -> True
      FD.Mem8 {} -> True
      FD.Mem16 {} -> True
      FD.Mem32 {} -> True
      FD.Mem64 {} -> True
      FD.Mem128 {} -> True
      FD.Mem256 {} -> True
      FD.FPMem32 {} -> True
      FD.FPMem64 {} -> True
      FD.FPMem80 {} -> True

      FD.ByteImm {} -> False
      FD.WordImm {} -> False
      FD.DWordImm {} -> False
      FD.QWordImm {} -> False
      FD.ByteSignedImm {} -> False
      FD.WordSignedImm {} -> False
      FD.DWordSignedImm {} -> False

      FD.SegmentValue {} -> False

type instance CruciblePersonality X86.X86_64 sym = MS.MacawSimulatorState sym

instance Architecture X86.X86_64 s where
  data ArchResult X86.X86_64 s =
    X86AnalysisResult (BinaryAnalysisResult s (C.Const Void) X86.X86_64)
  type CrucibleExt X86.X86_64 = MS.MacawExt X86.X86_64
--  type CruciblePersonality X86.X86_64 = forall sym . MS.MacawSimulatorState sym

  summarizeResult (AnalysisResult _ idx) = riSummary (O.runOnce idx)
  archNonce (AnalysisResult (X86AnalysisResult bar) _) = mcNonce bar
  genericSemantics _ _ = Nothing
  functions (AnalysisResult _ idx) = riFunctions (O.runOnce idx)
  containingBlocks (AnalysisResult (X86AnalysisResult bar) _) (X86Address addr) =
    mcContainingBlocks X86Address (toBlockX86 bar) bar addr
  functionBlocks (AnalysisResult (X86AnalysisResult ares) _) fh =
    let X86Address addr0 = fhAddress fh
        addr1 = MM.addrOffset addr0
        addr2 = R.concreteFromAbsolute addr1
    in mcFunctionBlocks (toBlockX86 ares fh) ares addr2
  alternativeIRs _ = [SomeIRRepr MacawRepr]
  asAlternativeIR repr ar@(AnalysisResult (X86AnalysisResult bar@BinaryAnalysisResult {rLoadedBinary = img}) _) fh =
    case repr of
      BaseRepr -> return Nothing
      MacawRepr -> do
        let mem = MBL.memoryImage img
        let blocks = [ (segOff, b)
                     | b <- functionBlocks ar fh
                     , let X86Address memAddr = blockAddress b
                     , Just segOff <- [MM.asSegmentOff mem memAddr]
                     ]
        let X86Address memAddr = fhAddress fh
        case MM.asAbsoluteAddr memAddr of
          Just memAbsAddr -> Just <$> AM.macawForBlocks X86Address (rNonceGen bar) (rBlockInfo bar) (R.concreteFromAbsolute memAbsAddr) blocks
          Nothing ->
            SCP.panic "X86 Macaw translation" ["Invalid address for function: " ++ show memAddr
                                              ]
      CrucibleRepr -> do
        let mem = MBL.memoryImage img
        let blocks = [ (segOff, b)
                     | b <- functionBlocks ar fh
                     , let X86Address memAddr = blockAddress b
                     , Just segOff <- [MM.asSegmentOff mem memAddr]
                     ]
        let X86Address memAddr = fhAddress fh
        case MM.asAbsoluteAddr memAddr of
          Just memAbsAddr -> AC.crucibleForMCBlocks (rNonceGen bar) (indexCrucibleMCBlocks X86Address) (rBlockInfo bar) (R.concreteFromAbsolute memAbsAddr) blocks
          Nothing ->
            SCP.panic "X86 Crucible translation" ["Invalid address for function: " ++ show memAddr
                                                 ]
  crucibleCFG (AnalysisResult (X86AnalysisResult bar) _) = mcCrucibleCFG x86addr64ToConcrete bar
  freshSymbolicEntry _ = mcFreshSymbolicEntry
  symbolicInitializers = mcSymbolicInitializers
  fromCrucibleBlock = Nothing

-- FIXME: This needs to be completed once the memory model rewrite for
-- macaw-symbolic is merged to master macaw.
mcSymbolicInitializers :: forall arch s sym
                        . ( CB.IsSymInterface sym
                          , MS.ArchInfo arch
                          , 1 <= MM.ArchAddrWidth arch
                          , CruciblePersonality arch sym ~ MS.MacawSimulatorState sym
                          , CrucibleExt arch ~ MS.MacawExt arch
                          )
                       => AnalysisResult arch s
                       -> sym
                       -> IO ( CS.IntrinsicTypes sym
                             , CFH.HandleAllocator
                             , CS.FunctionBindings (MS.MacawSimulatorState sym) sym (CrucibleExt arch)
                             , CS.ExtensionImpl (MS.MacawSimulatorState sym) sym (CrucibleExt arch)
                             , CruciblePersonality arch sym
                             )
mcSymbolicInitializers _ares sym = do
  let Just archVals = MS.archVals (Proxy @arch)
  let intrinsics = CLLI.llvmIntrinsicTypes
  halloc <- CFH.newHandleAllocator
  let bindings = CFH.emptyHandleMap
  let ?recordLLVMAnnotation = \_ _ -> return ()
  MS.withArchEval archVals sym $ \archEvalFns -> do
    -- These values are not hard to get, but it will be easier after a branch is
    -- merged from macaw that redoes some of the details of memory access.
    let gv = error "Global var for macaw"
    let globalMap = error "Global map for macaw"
    let lfh = error "lfh for macaw"
    let mkPtrValidity = error "mkpointervalidity predicate"
    let extImpl = MS.macawExtensions archEvalFns gv globalMap lfh mkPtrValidity
    return (intrinsics, halloc, bindings, extImpl, MS.MacawSimulatorState)

instance Eq (Address X86.X86_64 s) where
  X86Address a1 == X86Address a2 = a1 == a2

instance Ord (Address X86.X86_64 s) where
  X86Address a1 `compare` X86Address a2 = a1 `compare` a2

instance Show (Address X86.X86_64 s) where
  show (X86Address a) = show a

instance NFData (Address X86.X86_64 s) where
  rnf (X86Address a) = a `seq` ()

instance NFData (Instruction X86.X86_64 s) where
  rnf (X86Instruction i) = i `seq` ()

instance NFData (Operand X86.X86_64 s) where
  rnf (X86Operand _ _) = ()

-- This whole bit is unfortunate - we can probably export ppValue from flexdis
ppShowReg :: Show r => r -> PP.Doc ann
ppShowReg = PP.viaShow

ppValue :: Word64 -- ^ Base address for offset printing.
                  -- This should be the address of the next instruction.
        -> FD.Value
        -> PP.Doc ann
ppValue base v =
  case v of
    FD.ControlReg   r    -> PP.viaShow r
    FD.DebugReg     r    -> PP.viaShow r
    FD.MMXReg       r    -> PP.viaShow r
    FD.XMMReg       r    -> PP.viaShow r
    FD.YMMReg       r    -> PP.viaShow r
    FD.X87Register  n    -> PP.pretty "st" <> if n == 0 then mempty else PP.parens (PP.pretty n)
    FD.SegmentValue r    -> ppShowReg    r
    -- do the "*" belong here or in ppAddrRef?
    FD.FarPointer   addr -> PP.pretty "??FAR PTR??"            PP.<+> ppAddrRef    addr
    FD.VoidMem      addr -> ppAddrRef addr
    FD.Mem8         addr -> PP.pretty "BYTE PTR"    PP.<+> ppAddrRef addr
    FD.Mem16        addr -> PP.pretty "WORD PTR"    PP.<+> ppAddrRef addr
    FD.Mem32        addr -> PP.pretty "DWORD PTR"   PP.<+> ppAddrRef addr
    FD.Mem64        addr -> PP.pretty "QWORD PTR"   PP.<+> ppAddrRef addr
    FD.Mem128       addr -> PP.pretty "XMMWORD PTR" PP.<+> ppAddrRef addr
    FD.Mem256       addr -> PP.pretty "YMMWORD PTR" PP.<+> ppAddrRef addr
    FD.FPMem32      addr -> PP.pretty "DWORD PTR"   PP.<+> ppAddrRef addr
    FD.FPMem64      addr -> PP.pretty "QWORD PTR"   PP.<+> ppAddrRef addr
    FD.FPMem80      addr -> PP.pretty "TBYTE PTR"   PP.<+> ppAddrRef addr
    FD.ByteImm      imm  -> ppImm imm
    FD.WordImm      imm  -> ppImm imm
    FD.DWordImm     (FD.Imm32Concrete imm)  -> ppImm imm
    FD.DWordImm     (FD.Imm32SymbolOffset sym off _signed) ->
      PP.viaShow sym <> PP.pretty "+" <> ppImm off
    FD.QWordImm     (FD.UImm64Concrete imm)  -> ppImm imm
    FD.QWordImm     (FD.UImm64SymbolOffset symIdent off) ->
      PP.viaShow symIdent <> PP.pretty "@" <> ppImm off
    FD.ByteSignedImm i8 -> ppImm i8
    FD.WordSignedImm i16 -> ppImm i16
    FD.DWordSignedImm i32 -> ppImm i32
    FD.ByteReg      r    -> ppShowReg    r
    FD.WordReg      r    -> ppShowReg    r
    FD.DWordReg     r    -> ppShowReg    r
    FD.QWordReg     r    -> ppShowReg    r
    FD.JumpOffset _ (FD.FixedOffset off)  -> ppHex (base+fromIntegral off)
    FD.JumpOffset _ (FD.RelativeOffset ioff sym off) ->
      PP.viaShow sym <> PP.pretty "+" <> PP.pretty (off - fromIntegral (fromIntegral base + ioff))

ppHex :: (Integral a, Show a) => a -> PP.Doc ann
ppHex i = PP.pretty "0x" <> PP.pretty (showHex i "")

ppImm :: (Integral w, Show w) => w -> PP.Doc ann
ppImm i | i >= 0 = PP.pretty "0x" <> ppHex i
          -- Print negation after converting to integer
          -- Recall that  "negate minBound = minBound" with types like Int16, Int32, Int64.
        | otherwise = PP.pretty "-0x" <> ppHex (negate (toInteger i))

ppAddrRef :: FD.AddrRef -> PP.Doc ann
ppAddrRef addr =
  case addr of
    FD.Addr_32 seg base roff off ->
       case base of
         Just r | FD.isDefaultSeg32 seg r -> a
                | seg == FD.FS -> PP.viaShow seg <> (PP.colon PP.<+> a)
                | seg == FD.GS -> PP.viaShow seg <> (PP.colon PP.<+> a)
                | otherwise -> a -- ((HPJ.text (show seg) <> colon) <+>)
         _ -> a
      where a = ppAddr base roff off
                                                          -- or rip? this is 32 bits ...
    FD.IP_Offset_32 _seg off     -> PP.brackets $ PP.pretty "ip" <> appendDisplacement off
    FD.Offset_32 seg off         -> prefix seg off
    FD.Offset_64 seg off         -> prefix seg off
    FD.Addr_64 seg base roff off
        | seg == FD.FS || seg == FD.GS -> PP.viaShow seg <> PP.colon <> a
        | isDef     -> a
        | otherwise -> a
      where a = ppAddr base roff off
            isDef = maybe False (FD.isDefaultSeg64 seg) base

    FD.IP_Offset_64 _seg off -> PP.brackets $ PP.pretty "rip" <> appendDisplacement off
  where
    prefix seg off = ppShowReg seg <> PP.colon <> PP.viaShow off

    ppAddr :: Show r
           => Maybe r -- Base value
           -> Maybe (Int, r) -- Relative offset
           -> FD.Displacement -- Offset
           -> PP.Doc ann
    ppAddr base roff off =
      case (base, roff) of
         (Nothing, Nothing)     -> prettyDisplacement off
         (Nothing, Just (n, r)) ->
           PP.brackets (PP.viaShow r <> PP.pretty "*" <> PP.pretty n <> appendDisplacement off)
         (Just r, Nothing)      -> PP.brackets $
           PP.viaShow r <> appendDisplacement off
         (Just r, Just (n, r')) ->
           PP.brackets $
             PP.viaShow r <> PP.pretty "+" <> PP.viaShow r' <> PP.pretty "*" <> PP.pretty n <> appendDisplacement off

appendDisplacement :: FD.Displacement -> PP.Doc ann
appendDisplacement FD.NoDisplacement = mempty
appendDisplacement (FD.Disp32 (FD.Imm32Concrete x))
  | x >  0    = PP.pretty "+0x" <> ppHex x
  | x == 0    = mempty
  | otherwise = PP.pretty "-0x" <> ppHex (negate (fromIntegral x :: Int64))
appendDisplacement (FD.Disp32 off@(FD.Imm32SymbolOffset {})) =
  PP.pretty ("Unsupported offset: " ++ show off)
appendDisplacement (FD.Disp8 x)
  | x >  0    = PP.pretty "+0x" <> ppHex x
  | x == 0    = mempty
  | otherwise = PP.pretty "-0x" <> ppHex (negate (fromIntegral x :: Int16))

prettyDisplacement :: FD.Displacement -> PP.Doc ann
prettyDisplacement FD.NoDisplacement = PP.pretty "0"
prettyDisplacement (FD.Disp32 (FD.Imm32Concrete x)) =
  if x >= 0 then
    PP.pretty "0x" <> ppHex x
   else
    PP.pretty "-0x" <> ppHex (negate (fromIntegral x :: Int64))
prettyDisplacement (FD.Disp32 off@(FD.Imm32SymbolOffset {})) =
  PP.pretty "Unsupported offset: " <> PP.viaShow off
prettyDisplacement (FD.Disp8 x) =
  if x >= 0 then
    PP.pretty "0x" <> ppHex x
   else
    PP.pretty "-0x" <> ppHex (negate (fromIntegral x :: Int16))

-- Symbolic execution support

mcFreshSymbolicEntry :: (CB.IsSymInterface sym) => sym -> CT.TypeRepr tp -> Maybe (IO (CS.RegValue sym tp))
mcFreshSymbolicEntry sym rep =
  case rep of
    LLM.LLVMPointerRepr w -> return $ do
      -- We allocate just plain bitvectors for LLVMPointer, as fully symbolic
      -- pointers aren't useful and users have to provide more interesting
      -- pointer relations manually.
      bv <- WI.freshConstant sym (WS.safeSymbol "bv") (WI.BaseBVRepr w)
      LLM.llvmPointer_bv sym bv
    _ -> Nothing
