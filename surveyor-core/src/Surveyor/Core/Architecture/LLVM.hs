-- | An implementation of 'Architecture' for LLVM bitcode
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Surveyor.Core.Architecture.LLVM ( mkLLVMResult ) where

import           Control.DeepSeq ( NFData, rnf )
import qualified Control.Exception as X
import           Control.Lens ( (^.) )
import qualified Control.Lens as L
import           Control.Monad ( guard )
import qualified Control.Once as O
import qualified Data.Foldable as F
import qualified Data.Map.Strict as M
import           Data.Maybe ( catMaybes, isJust, fromMaybe, mapMaybe )
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.Nonce as NG
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Parameterized.TraversableFC as FC
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Lang.Crucible.CFG.Core as C
import qualified Lang.Crucible.CFG.Extension as CCE
import qualified Lang.Crucible.FunctionHandle as CFH
import qualified Lang.Crucible.LLVM.Bytes as CLLB
import qualified Lang.Crucible.LLVM.DataLayout as CLDL
import qualified Lang.Crucible.LLVM.Extension as LE
import qualified Lang.Crucible.LLVM.MemModel as LLM
import qualified Lang.Crucible.LLVM.Translation as LT
import qualified Lang.Crucible.LLVM.TypeContext as LTC
import           System.FilePath ( (</>) )
import qualified System.FilePath as SFP
import qualified Text.LLVM as LL
import qualified Text.LLVM.PP as LL
import qualified Text.PrettyPrint as PP
import           Text.Printf ( printf )
import qualified What4.ProgramLoc as WPL

import           Surveyor.Core.Architecture.Class
import qualified Surveyor.Core.Architecture.Crucible as AC
import           Surveyor.Core.IRRepr ( IRRepr(MacawRepr, BaseRepr, CrucibleRepr), Crucible )

data LLVM

data LLVMResult s =
  LLVMResult { llvmNonce :: NG.Nonce s LLVM
             , llvmModule :: LL.Module
             , llvmFunctionIndex :: FunctionIndex s
             , llvmNonceGen :: NG.NonceGenerator IO s
             , llvmHdlAlloc :: CFH.HandleAllocator
             -- ^ The handle allocator used for the Crucible translation - this needs to be kept
             -- around to symbolically simulate later
             , llvmCrucibleTranslation :: Some LT.ModuleTranslation
             -- ^ The (cached) Crucible translation
             }

type FunctionIndex s = M.Map LL.Symbol (FunctionHandle LLVM s, LL.Define, BlockIndex)

-- | Mapping from block labels to their blocks, along with the computed position
-- information for each block (derived from debug information)
data BlockIndex =
  BlockIndex { biLabelToBlock :: M.Map (Maybe LL.BlockLabel) LL.BasicBlock
             , biPosToBlock :: M.Map WPL.Position LL.BasicBlock
             }
-- type BlockIndex = M.Map (Maybe LL.BlockLabel) (LL.BasicBlock, Maybe WPL.Position)

indexFunctions :: LL.Module -> FunctionIndex s
indexFunctions llvmmodule = F.foldl' indexDefine M.empty (LL.modDefines llvmmodule)
  where
    (_, ltc) = LTC.typeContextFromModule llvmmodule
    indexDefine m def
      | null (LL.defBody def) = m
      | otherwise  =
        let blockIndex = BlockIndex { biLabelToBlock = F.foldl' indexBlock M.empty (LL.defBody def)
                                    , biPosToBlock = F.foldl' indexPosition M.empty (LL.defBody def)
                                    }
            fh = FunctionHandle { fhAddress = LLVMAddress (FunctionAddr (LL.defName def))
                                , fhName = T.pack (show (LL.ppSymbol (LL.defName def)))
                                }
        in M.insert (LL.defName def) (fh, def, blockIndex) m
    indexBlock m b = M.insert (LL.bbLabel b) b m
    indexPosition m b =
      maybe m (\p -> M.insert p b m) (blockPosition ltc b)

blockPosition :: LTC.TypeContext -> LL.BasicBlock -> Maybe WPL.Position
blockPosition ltc bb0 =
  let ?lc = ltc
  in go (LL.bbStmts bb0)
  where
    go [] = Nothing
    go (stmt:stmts) =
      case stmt of
        LL.Result _ _ md -> extractLoc md stmts
        LL.Effect _ md -> extractLoc md stmts
    extractLoc [] stmts = go stmts
    extractLoc (md:mds) stmts =
      case md of
        ("dbg", LL.ValMdLoc dl) -> do
          let ln = fromIntegral (LL.dlLine dl)
          let col = fromIntegral (LL.dlCol dl)
          let file = getFile (LL.dlScope dl)
          return (WPL.SourcePos file ln col)
        ("dbg", LL.ValMdDebugInfo (LL.DebugInfoSubprogram subp))
          | Just file' <- LL.dispFile subp -> do
              let ln = fromIntegral (LL.dispLine subp)
              let file = getFile file'
              return (WPL.SourcePos file ln 0)
        _ -> extractLoc mds stmts

    getFile :: (?lc :: LTC.TypeContext) => LL.ValMd -> T.Text
    getFile = T.pack . maybe "" filenm . findFile

    filenm di
      | SFP.isRelative (LL.difFilename di) = LL.difDirectory di </> LL.difFilename di
      | otherwise = LL.difFilename di

    findFile :: (?lc :: LTC.TypeContext) => LL.ValMd -> Maybe LL.DIFile
    findFile (LL.ValMdRef x) = findFile =<< lookupMetadata x

    findFile (LL.ValMdNode (_:Just (LL.ValMdRef y):_)) =
      case lookupMetadata y of
        Just (LL.ValMdNode [Just (LL.ValMdString fname), Just (LL.ValMdString fpath)]) ->
            Just (LL.DIFile fname fpath)
        _ -> Nothing

    findFile (LL.ValMdDebugInfo di) =
      case di of
        LL.DebugInfoFile dif -> Just dif
        LL.DebugInfoLexicalBlock dilex
          | Just md <- LL.dilbFile dilex -> findFile md
          | Just md <- LL.dilbScope dilex -> findFile md
        LL.DebugInfoLexicalBlockFile dilexFile
          | Just md <- LL.dilbfFile dilexFile -> findFile md
          | otherwise -> findFile (LL.dilbfScope dilexFile)
        LL.DebugInfoSubprogram disub
          | Just md <- LL.dispFile disub -> findFile md
          | Just md <- LL.dispScope disub -> findFile md
        _ -> Nothing

    findFile _ = Nothing

    lookupMetadata :: (?lc :: LTC.TypeContext) => Int -> Maybe LL.ValMd
    lookupMetadata x = L.view (L.at x) (LTC.llvmMetadataMap ?lc)


mkLLVMResult :: NG.NonceGenerator IO s
             -> NG.Nonce s LLVM
             -> CFH.HandleAllocator
             -> LL.Module
             -> IO (SomeResult s LLVM)
mkLLVMResult ng nonce hdlAlloc m = do
  let ?laxArith = True
  ct <- LT.translateModule hdlAlloc m
  let lr = LLVMResult { llvmNonce = nonce
                      , llvmModule = m
                      , llvmFunctionIndex = indexFunctions m
                      , llvmHdlAlloc = hdlAlloc
                      , llvmCrucibleTranslation = ct
                      , llvmNonceGen = ng
                      }
  return (SomeResult (AnalysisResult (LLVMAnalysisResult lr) (indexResult lr)))

indexResult :: LLVMResult s -> O.Once (ResultIndex LLVM s)
indexResult lr = O.once idx
  where
    idx = ResultIndex { riFunctions = mapMaybe defToFunction (LL.modDefines (llvmModule lr))
                      , riSummary = summarizeModule (llvmModule lr)
                      }

data AddrType = FuncK | BlockK | InsnK

-- | The type of an LLVM address
--
-- There isn't really an address type. Blocks have BlockLabels and the name of
-- the containing function, while instructions can be addressed by tuples of
-- (Symbol, BlockLabel, Int) where the Int is an offset into the block.
-- Function addresses are just names, since they have to be unique.  For names,
-- we use the LLVM symbols associated with them, which are uniqued strings.
data Addr addrTy where
  FunctionAddr :: !LL.Symbol -> Addr 'FuncK
  BlockAddr :: !(Addr 'FuncK) -> Maybe LL.BlockLabel -> Addr 'BlockK
  InsnAddr :: !(Addr 'BlockK) -> !Int -> Addr 'InsnK

instance Eq (Addr addrTy) where
  a1 == a2 = isJust (testEquality a1 a2)

instance Ord (Addr addrTy) where
  compare a1 a2 = toOrdering (compareF a1 a2)

deriving instance Show (Addr addrTy)

instance TestEquality Addr where
  testEquality a1 a2 =
    case a1 of
      FunctionAddr s1 ->
        case a2 of
          FunctionAddr s2 -> do
            guard (s1 == s2)
            return Refl
          _ -> Nothing
      BlockAddr f1 b1 ->
        case a2 of
          BlockAddr f2 b2 -> do
            Refl <- testEquality f1 f2
            guard (b1 == b2)
            return Refl
          _ -> Nothing
      InsnAddr b1 i1 ->
        case a2 of
          InsnAddr b2 i2 -> do
            Refl <- testEquality b1 b2
            guard (i1 == i2)
            return Refl
          _ -> Nothing

instance OrdF Addr where
  compareF a1 a2 =
    case a1 of
      FunctionAddr s1 ->
        case a2 of
          FunctionAddr s2 -> fromOrdering (compare s1 s2)
          BlockAddr {} -> GTF
          InsnAddr {} -> GTF
      BlockAddr f1 b1 ->
        case a2 of
          FunctionAddr {} -> LTF
          BlockAddr f2 b2 ->
            case compareF f1 f2 of
              EQF -> fromOrdering (compare b1 b2)
              GTF -> GTF
              LTF -> LTF
          InsnAddr {} -> GTF
      InsnAddr b1 i1 ->
        case a2 of
          FunctionAddr {} -> LTF
          BlockAddr {} -> LTF
          InsnAddr b2 i2 ->
            case compareF b1 b2 of
              EQF -> fromOrdering (compare i1 i2)
              GTF -> GTF
              LTF -> LTF

data LLVMOperand' = Value !LL.Value
                  | TypedValue !(LL.Typed LL.Value)
                  | Type !LL.Type
                  | ConstantInt !Int
                  | BlockLabel !LL.BlockLabel
                  | ConstantString String
                  | SwitchTarget LL.Type (Integer, LL.BlockLabel)
                  | Ordering LL.AtomicOrdering
                  | AtomicOp LL.AtomicRWOp

instance IR LLVM s where
  data Instruction LLVM s = LLVMInstruction LL.Stmt
  data Operand LLVM s = LLVMOperand LLVMOperand'
  data Opcode LLVM s = LLVMOpcode LL.Instr
  data Address LLVM s = forall addrTy . LLVMAddress (Addr addrTy)
  boundValue (LLVMInstruction stmt) =
    case stmt of
      LL.Result iden _ _ -> Just (LLVMOperand (Value (LL.ValIdent iden)))
      LL.Effect {} -> Nothing
  prettyOperand (LLVMAddress _addr) (LLVMOperand val) =
    let ?config = llvmConfig
    in T.pack (show (ppOperand val)) -- T.pack (show (LL.ppValue val))
  prettyAddress (LLVMAddress addr) =
    case addr of
      FunctionAddr (LL.Symbol name) -> T.pack name
      BlockAddr (FunctionAddr (LL.Symbol name)) l ->
        T.pack (printf "%s%s" name (maybe "" (("@"++) . show . LL.ppLabel) l))
      InsnAddr (BlockAddr (FunctionAddr (LL.Symbol name)) l) i -> T.pack (printf "%s%s:%d" name (maybe "" (("@"++) . show . LL.ppLabel) l) i)
  prettyInstruction _ (LLVMInstruction stmt) =
    let ?config = llvmConfig
    in T.pack (show (LL.ppStmt stmt))
  opcode (LLVMInstruction stmt) =
    case stmt of
      LL.Result _ i _ -> LLVMOpcode i
      LL.Effect i _ -> LLVMOpcode i
  prettyOpcode (LLVMOpcode i) = ppOpcode i
  operands (LLVMInstruction stmt) = stmtOperands stmt
  -- Will work on what this means - we can probably do something if we also pass
  -- in the analysisresult
  parseAddress _ = Nothing
  rawRepr = Nothing
  showInstructionAddresses _ = False
  operandSelectable (LLVMOperand o) =
    case o of
      Value {} -> True
      TypedValue {} -> True
      -- So that we can quick jump to blocks
      BlockLabel {} -> True
      -- Same
      SwitchTarget {} -> True
      -- Not selectable
      Type {} -> False
      ConstantInt {} -> False
      ConstantString {} -> False
      Ordering {} -> False
      AtomicOp {} -> False

instance Architecture LLVM s where
  data ArchResult LLVM s = LLVMAnalysisResult (LLVMResult s)
  archNonce (AnalysisResult (LLVMAnalysisResult lr) _) = llvmNonce lr
  genericSemantics _ _ = Nothing
  functions (AnalysisResult _ idx) = riFunctions (O.runOnce idx)
  containingBlocks (AnalysisResult (LLVMAnalysisResult lr) _) (LLVMAddress addr) =
    llvmContainingBlocks lr addr
  summarizeResult (AnalysisResult _ idx) = riSummary (O.runOnce idx)
  functionBlocks (AnalysisResult (LLVMAnalysisResult lr) _) fh =
    llvmFunctionBlocks lr fh
  alternativeIRs _ = [SomeIRRepr CrucibleRepr]
  asAlternativeIR repr (AnalysisResult (LLVMAnalysisResult llr) _) fh =
    case repr of
      BaseRepr -> return Nothing
      MacawRepr -> return Nothing
      CrucibleRepr ->
        crucibleForLLVMBlocks (llvmNonceGen llr) (llvmFunctionIndex llr) fh (llvmCrucibleTranslation llr)

data CrucibleLLVMOperand arch s where
  Alignment :: CLDL.Alignment -> CrucibleLLVMOperand arch s
  StringLiteral :: String -> CrucibleLLVMOperand arch s
  StorageType :: LLM.StorageType -> CrucibleLLVMOperand arch s
  Bytes :: CLLB.Bytes -> CrucibleLLVMOperand arch s
  GlobalSymbol :: LLM.GlobalSymbol -> CrucibleLLVMOperand arch s

-- NOTE: We only support x86/llvm right now (since crucible-llvm only supports that)
--
-- If that changes, we'll need to extend our own LLVM type (or possibly just re-use the
-- crucible-llvm one)
instance AC.CrucibleExtension LLVM where
  type CrucibleExt LLVM = LE.LLVM (LE.X86 64)
  type CrucibleExtensionOperand LLVM = CrucibleLLVMOperand (LE.LLVM (LE.X86 64))

  prettyExtensionStmt _ = prettyLLVMStmt
  prettyExtensionApp _ = prettyLLVMApp
  prettyExtensionOperand _ = prettyLLVMExtensionOperand
  extensionExprOperands = llvmExtensionExprOperands
  extensionStmtOperands = llvmExtensionStmtOperands
  -- FIXME: Figure out which kinds of LLVM-specific operands can be selected
  extensionOperandSelectable _ _ = False

prettyLLVMExtensionOperand :: CrucibleLLVMOperand arch s -> T.Text
prettyLLVMExtensionOperand o =
  case o of
    Alignment a -> T.pack (show a)
    StringLiteral s -> T.pack (show s)
    StorageType t -> T.pack (show t)
    Bytes b -> T.pack (show b)
    GlobalSymbol sym -> T.pack (show sym)

prettyLLVMStmt :: CCE.StmtExtension (LE.LLVM (LE.X86 64)) (C.Reg ctx) tp -> T.Text
prettyLLVMStmt s =
  case s of
    LE.LLVM_PushFrame {} -> "llvm.push-frame"
    LE.LLVM_PopFrame {} -> "llvm.pop-frame"
    LE.LLVM_Alloca {} -> "llvm.alloca"
    LE.LLVM_Load {} -> "llvm.load"
    LE.LLVM_Store {} -> "llvm.store"
    LE.LLVM_MemClear {} -> "llvm.mem-clear"
    LE.LLVM_LoadHandle {} -> "llvm.load-handle"
    LE.LLVM_ResolveGlobal {} -> "llvm.load-global"
    LE.LLVM_PtrEq {} -> "llvm.ptr-eq"
    LE.LLVM_PtrLe {} -> "llvm.ptr-le"
    LE.LLVM_PtrAddOffset {} -> "llvm.ptr-add-offset"
    LE.LLVM_PtrSubtract {} -> "llvm.ptr-subtract"

prettyLLVMApp :: CCE.ExprExtension (LE.LLVM (LE.X86 64)) (C.Reg ctx) tp -> T.Text
prettyLLVMApp a =
  case a of
    LE.LLVM_PointerExpr {} -> "llvm.pointer-expr"
    LE.LLVM_PointerBlock {} -> "llvm.pointer-block"
    LE.LLVM_PointerOffset {} -> "llvm.pointer-offset"
    LE.LLVM_PointerIte {} -> "llvm.pointer-ite"
    -- FIXME: Probably need to examine the x86.ExtX86 here
    LE.X86Expr {} -> "llvm.x86-expr"

llvmExtensionExprOperands :: AC.NonceCache s ctx
                          -> NG.NonceGenerator IO s
                          -> CCE.ExprExtension (LE.LLVM (LE.X86 64)) (C.Reg ctx) tp
                          -> IO [Operand (AC.Crucible LLVM) s]
llvmExtensionExprOperands cache ng e =
  case e of
    -- FIXME: Walk the subterm
    LE.X86Expr x86e -> return []
    LE.LLVM_PointerExpr nr r1 r2 -> do
      n1 <- NG.freshNonce ng
      return [ AC.CrucibleOperand n1 (AC.NatRepr nr)
             , AC.toRegisterOperand cache r1
             , AC.toRegisterOperand cache r2
             ]
    LE.LLVM_PointerBlock nr r -> do
      n1 <- NG.freshNonce ng
      return [ AC.CrucibleOperand n1 (AC.NatRepr nr)
             , AC.toRegisterOperand cache r
             ]
    LE.LLVM_PointerOffset nr r -> do
      n1 <- NG.freshNonce ng
      return [ AC.CrucibleOperand n1 (AC.NatRepr nr)
             , AC.toRegisterOperand cache r
             ]
    LE.LLVM_PointerIte nr r1 r2 r3 -> do
      n1 <- NG.freshNonce ng
      return [ AC.CrucibleOperand n1 (AC.NatRepr nr)
             , AC.toRegisterOperand cache r1
             , AC.toRegisterOperand cache r2
             , AC.toRegisterOperand cache r3
             ]

llvmExtensionStmtOperands :: AC.NonceCache s ctx
                          -> NG.NonceGenerator IO s
                          -> CCE.StmtExtension (LE.LLVM (LE.X86 64)) (C.Reg ctx) tp
                          -> IO [Operand (AC.Crucible LLVM) s]
llvmExtensionStmtOperands cache ng s =
  case s of
    LE.LLVM_PushFrame gv -> do
      n1 <- NG.freshNonce ng
      return [ AC.CrucibleOperand n1 (AC.GlobalVar gv) ]
    LE.LLVM_PopFrame gv -> do
      n1 <- NG.freshNonce ng
      return [ AC.CrucibleOperand n1 (AC.GlobalVar gv) ]
    LE.LLVM_Alloca nr gv r align locStr -> do
      n1 <- NG.freshNonce ng
      n2 <- NG.freshNonce ng
      n3 <- NG.freshNonce ng
      n4 <- NG.freshNonce ng
      return [ AC.CrucibleOperand n1 (AC.NatRepr nr)
             , AC.CrucibleOperand n2 (AC.GlobalVar gv)
             , AC.toRegisterOperand cache r
             , AC.toExtensionOperand n3 (Alignment align)
             , AC.toExtensionOperand n4 (StringLiteral locStr)
             ]
    LE.LLVM_Load gv r trep storageType align -> do
      n1 <- NG.freshNonce ng
      n2 <- NG.freshNonce ng
      n3 <- NG.freshNonce ng
      n4 <- NG.freshNonce ng
      return [ AC.CrucibleOperand n1 (AC.GlobalVar gv)
             , AC.toRegisterOperand cache r
             , AC.CrucibleOperand n2 (AC.TypeRepr trep)
             , AC.toExtensionOperand n3 (StorageType storageType)
             , AC.toExtensionOperand n4 (Alignment align)
             ]
    LE.LLVM_Store gv r1 trep storageType align r2 -> do
      n1 <- NG.freshNonce ng
      n2 <- NG.freshNonce ng
      n3 <- NG.freshNonce ng
      n4 <- NG.freshNonce ng
      return [ AC.CrucibleOperand n1 (AC.GlobalVar gv)
             , AC.toRegisterOperand cache r1
             , AC.CrucibleOperand n2 (AC.TypeRepr trep)
             , AC.toExtensionOperand n3 (StorageType storageType)
             , AC.toExtensionOperand n4 (Alignment align)
             , AC.toRegisterOperand cache r2
             ]
    LE.LLVM_MemClear gv r bytes -> do
      n1 <- NG.freshNonce ng
      n2 <- NG.freshNonce ng
      return [ AC.CrucibleOperand n1 (AC.GlobalVar gv)
             , AC.toRegisterOperand cache r
             , AC.toExtensionOperand n2 (Bytes bytes)
             ]
    LE.LLVM_LoadHandle gv r argReprs retRepr -> do
      n1 <- NG.freshNonce ng
      n2 <- NG.freshNonce ng
      n3 <- NG.freshNonce ng
      return [ AC.CrucibleOperand n1 (AC.GlobalVar gv)
             , AC.toRegisterOperand cache r
             , AC.CrucibleOperand n2 (AC.CtxRepr argReprs)
             , AC.CrucibleOperand n3 (AC.TypeRepr retRepr)
             ]
    LE.LLVM_ResolveGlobal nr gv gsym -> do
      n1 <- NG.freshNonce ng
      n2 <- NG.freshNonce ng
      n3 <- NG.freshNonce ng
      return [ AC.CrucibleOperand n1 (AC.NatRepr nr)
             , AC.CrucibleOperand n2 (AC.GlobalVar gv)
             , AC.toExtensionOperand n3 (GlobalSymbol gsym)
             ]
    LE.LLVM_PtrEq gv r1 r2 -> do
      n1 <- NG.freshNonce ng
      return [ AC.CrucibleOperand n1 (AC.GlobalVar gv)
             , AC.toRegisterOperand cache r1
             , AC.toRegisterOperand cache r2
             ]
    LE.LLVM_PtrLe gv r1 r2 -> do
      n1 <- NG.freshNonce ng
      return [ AC.CrucibleOperand n1 (AC.GlobalVar gv)
             , AC.toRegisterOperand cache r1
             , AC.toRegisterOperand cache r2
             ]
    LE.LLVM_PtrAddOffset nr gv r1 r2 -> do
      n1 <- NG.freshNonce ng
      n2 <- NG.freshNonce ng
      return [ AC.CrucibleOperand n1 (AC.NatRepr nr)
             , AC.CrucibleOperand n2 (AC.GlobalVar gv)
             , AC.toRegisterOperand cache r1
             , AC.toRegisterOperand cache r2
             ]
    LE.LLVM_PtrSubtract nr gv r1 r2 -> do
      n1 <- NG.freshNonce ng
      n2 <- NG.freshNonce ng
      return [ AC.CrucibleOperand n1 (AC.NatRepr nr)
             , AC.CrucibleOperand n2 (AC.GlobalVar gv)
             , AC.toRegisterOperand cache r1
             , AC.toRegisterOperand cache r2
             ]

data LLVMException where
  InvalidFunctionAddress :: Addr addrTy -> LLVMException
  MissingIndexForFunction :: LL.Symbol -> LLVMException
  UnsupportedLLVMArchitecture :: LE.ArchRepr arch -> LLVMException

-- We need a custom instance here because there is no Show instance for ArchReprs
instance Show LLVMException where
  show e =
    case e of
      InvalidFunctionAddress addr -> "InvalidFunctionAddress " ++ show addr
      MissingIndexForFunction sym -> "MissingIndexForFunction " ++ show sym
      UnsupportedLLVMArchitecture arep ->
        case arep of
          LE.X86Repr nr -> "UnsupportedLLVMArchitecture X86 " ++ show nr

instance X.Exception LLVMException

crucibleForLLVMBlocks :: forall s
                       . NG.NonceGenerator IO s
                      -> FunctionIndex s
                      -> FunctionHandle LLVM s
                      -> Some LT.ModuleTranslation
                      -> IO (Maybe (BlockMapping LLVM (Crucible LLVM) s))
crucibleForLLVMBlocks ng funcIndex fh (Some mt) =
  case fhAddress fh of
    LLVMAddress fa@(FunctionAddr sym) ->
      case M.lookup sym (LT.cfgMap mt) of
        Nothing -> return Nothing
        Just (C.AnyCFG cfg) -> do
          case M.lookup sym funcIndex of
            Nothing -> X.throwIO (MissingIndexForFunction sym)
            Just (_, _, blockIndex) -> do
              case LT.llvmArch (mt ^. LT.transContext) of
                LE.X86Repr wrep
                  | Just Refl <- testEquality wrep (C.knownNat @64) -> do
                      let bm0 :: BlockMapping LLVM (Crucible LLVM) s
                          bm0 = BlockMapping mempty mempty mempty
                      Just <$> FC.foldlMFC' (toLLVMCrucibleBlock ng blockIndex fa fh cfg) bm0 (C.cfgBlockMap cfg)
                arep -> X.throwIO (UnsupportedLLVMArchitecture arep)
    LLVMAddress a -> X.throwIO (InvalidFunctionAddress a)

toLLVMCrucibleBlock :: forall s blocks init ret ctx (arch :: LE.LLVMArch)
                     . (arch ~ LE.X86 64)
                    => NG.NonceGenerator IO s
                    -> BlockIndex
                    -> Addr 'FuncK
                    -> FunctionHandle LLVM s
                    -> C.CFG (LE.LLVM arch) blocks init ret
                    -> BlockMapping LLVM (Crucible LLVM) s
                    -> C.Block (LE.LLVM arch) blocks ret ctx
                    -> IO (BlockMapping LLVM (Crucible LLVM) s)
toLLVMCrucibleBlock ng blockIndex (FunctionAddr sym) fh cfg bm crucBlock = do
  let hdl = CFH.handleID (C.cfgHandle cfg)
  let hdlNum = NG.indexValue hdl
  case M.lookup (WPL.plSourceLoc (C.blockLoc crucBlock)) (biPosToBlock blockIndex) of
    Nothing -> return bm
    Just llvmBlock -> do
      c0 <- AC.initialCache ng (C.blockInputs crucBlock)
      -- We use the nonce ID of the CFG for the function address; it is stable
      -- and probably as good as any other address we could give.  Maybe it
      -- would be better to generalize the type of function addresses for
      -- Crucible and have it be a string for LLVM/Crucible?
      let baddr = AC.BlockAddr (AC.FunctionAddr hdlNum) (Ctx.indexVal (C.blockIDIndex (C.blockID crucBlock)))
      stmts <- buildBlock c0 baddr 0 (crucBlock ^. C.blockStmts)
      let crucAddr = FunctionHandle { fhName = fhName fh
                                    , fhAddress = AC.CrucibleAddress (AC.FunctionAddr hdlNum)
                                    }
      let crucBlock' = Block { blockAddress = AC.CrucibleAddress baddr
                             , blockInstructions = stmts
                             , blockFunction = crucAddr
                             }
      let llvmBlock' = toBlock fh sym llvmBlock

      return BlockMapping { blockMapping = M.insert (blockAddress llvmBlock') (llvmBlock', crucBlock') (blockMapping bm)
                          , baseToIRAddrs = M.insert (blockAddress llvmBlock') (S.singleton (blockAddress crucBlock')) (baseToIRAddrs bm)
                          , irToBaseAddrs = M.insert (blockAddress crucBlock') (S.singleton (blockAddress llvmBlock')) (irToBaseAddrs bm)
                          }
  where
    buildBlock :: forall ctx'
                . AC.NonceCache s ctx'
               -> AC.Addr 'AC.BlockK
               -> Int
               -> C.StmtSeq (LE.LLVM arch) blocks ret ctx'
               -> IO [(AC.Address (Crucible LLVM) s, Instruction (Crucible LLVM) s)]
    buildBlock nc baddr iidx ss =
      case ss of
        C.ConsStmt _loc stmt ss' -> do
          (nc', mBinder, ops) <- AC.crucibleStmtOperands nc ng stmt
          rest <- buildBlock nc' baddr (iidx + 1) ss'
          let sz = AC.cacheSize nc
          let iaddr = AC.CrucibleAddress (AC.InstructionAddr baddr iidx)
          let cstmt = AC.CrucibleStmt sz stmt mBinder ops
          return ((iaddr, cstmt) : rest )
        C.TermStmt _loc term -> do
          ops <- AC.crucibleTermStmtOperands nc ng term
          let iaddr = AC.CrucibleAddress (AC.InstructionAddr baddr iidx)
          let cstmt = AC.CrucibleTermStmt term ops
          return [(iaddr, cstmt)]

instance Eq (Address LLVM s) where
  LLVMAddress a1 == LLVMAddress a2 = isJust (testEquality a1 a2)

instance Ord (Address LLVM s) where
  compare (LLVMAddress a1) (LLVMAddress a2) = toOrdering (compareF a1 a2)

instance Show (Address LLVM s) where
  show (LLVMAddress a) = show a

instance NFData (Address LLVM s) where
  rnf (LLVMAddress a) = a `seq` ()

-- FIXME: This could be improved if we changed the underlying llvm-pretty definition
instance NFData (Instruction LLVM s) where
  rnf (LLVMInstruction i) = i `seq` ()

instance NFData (Operand LLVM s) where
  rnf (LLVMOperand o) = o `seq` ()

ppOperand :: (?config :: LL.Config) => LLVMOperand' -> PP.Doc
ppOperand op =
  case op of
    Value v -> LL.ppValue v
    TypedValue tv -> LL.ppTyped LL.ppValue tv
    Type ty -> LL.ppType ty
    ConstantInt i -> PP.int i
    BlockLabel l -> LL.ppLabel l
    ConstantString s -> PP.text s
    SwitchTarget t (val, target) -> LL.ppSwitchEntry t (val, target)
    Ordering ao -> LL.ppAtomicOrdering ao
    AtomicOp ao -> LL.ppAtomicOp ao

stmtOperands :: LL.Stmt -> OperandList (Operand LLVM s)
stmtOperands stmt =
  case stmt of
    LL.Result _ instr _ -> instrOperands instr
    LL.Effect instr _ -> instrOperands instr

instrOperands :: LL.Instr -> OperandList (Operand LLVM s)
instrOperands i =
  case i of
    LL.RetVoid {} -> fromList []
    LL.Ret rv -> fromList [LLVMOperand (TypedValue rv)]
    LL.Arith _ tv v ->
      fromList [ LLVMOperand (TypedValue tv)
               , LLVMOperand (Value v)
               ]
    LL.Bit _ tv v ->
      fromList [ LLVMOperand (TypedValue tv)
               , LLVMOperand (Value v)
               ]
    LL.Conv _ tv ty ->
      fromList [ LLVMOperand (TypedValue tv)
               , LLVMOperand (Type ty)
               ]
    LL.Call _ ty callee args ->
      fromItemList [ Item (LLVMOperand (Type ty))
                   , Item (LLVMOperand (Value callee))
                   , Delimited Parens (fromList (map (LLVMOperand . TypedValue) args))
                   ]
    LL.Alloca ty nelts align ->
      fromList $
      concat [ [LLVMOperand (Type ty)]
             , maybe [] ((:[]) . LLVMOperand . TypedValue) nelts
             , maybe [] ((:[]) . LLVMOperand . ConstantInt) align
             ]
    LL.Load tv _ align ->
      fromList $
      concat [ [LLVMOperand (TypedValue tv)]
             , maybe [] ((:[]) . LLVMOperand . ConstantInt) align
             ]
    LL.Store tv1 tv2 ordering align ->
      fromList $
      concat [ [LLVMOperand (TypedValue tv1), LLVMOperand (TypedValue tv2)]
             , maybe [] ((:[]) . LLVMOperand . Ordering) ordering
             , maybe [] ((:[]) . LLVMOperand . ConstantInt) align
             ]
    LL.ICmp _ tv v -> fromList [LLVMOperand (TypedValue tv), LLVMOperand (Value v)]
    LL.FCmp _ tv v -> fromList [LLVMOperand (TypedValue tv), LLVMOperand (Value v)]
    LL.Phi ty vs ->
      fromItemList [ Item (LLVMOperand (Type ty))
                   , Delimited Parens (fromList (map (LLVMOperand . Value . fst) vs))
                   ]
    LL.GEP _ tv tvs -> fromList (LLVMOperand (TypedValue tv) : map (LLVMOperand . TypedValue) tvs)
    LL.Select tv1 tv2 v ->
      fromList [ LLVMOperand (TypedValue tv1)
               , LLVMOperand (TypedValue tv2)
               , LLVMOperand (Value v)
               ]
    LL.ExtractValue tv ixs -> fromList (LLVMOperand (TypedValue tv) : map (LLVMOperand . ConstantInt . fromIntegral) ixs)
    LL.InsertValue tv1 tv2 ixs ->
      fromList (LLVMOperand (TypedValue tv1) : LLVMOperand (TypedValue tv2) : map (LLVMOperand . ConstantInt . fromIntegral) ixs)
    LL.ExtractElt tv v -> fromList [LLVMOperand (TypedValue tv), LLVMOperand (Value v)]
    LL.InsertElt tv1 tv2 v ->
      fromList [ LLVMOperand (TypedValue tv1)
               , LLVMOperand (TypedValue tv2)
               , LLVMOperand (Value v)
               ]
    LL.ShuffleVector tv1 v tv2 ->
      fromList [ LLVMOperand (TypedValue tv1)
               , LLVMOperand (Value v)
               , LLVMOperand (TypedValue tv2)
               ]
    LL.Jump lab -> fromList [ LLVMOperand (BlockLabel lab) ]
    LL.Br tv l1 l2 ->
      fromList [ LLVMOperand (TypedValue tv)
               , LLVMOperand (BlockLabel l1)
               , LLVMOperand (BlockLabel l2)
               ]
    LL.Invoke ty v tvs l1 l2 ->
      fromItemList [ Item (LLVMOperand (Type ty))
                   , Item (LLVMOperand (Value v))
                   , Delimited Parens (fromList (map (LLVMOperand . TypedValue) tvs))
                   , Item (LLVMOperand (BlockLabel l1))
                   , Item (LLVMOperand (BlockLabel l2))
                   ]
    LL.Comment s -> fromList [ LLVMOperand (ConstantString s) ]
    LL.Unreachable -> fromList []
    LL.Unwind -> fromList []
    LL.VaArg tv t -> fromList [ LLVMOperand (TypedValue tv), LLVMOperand (Type t) ]
    LL.IndirectBr tv labs ->
      fromItemList [ Item (LLVMOperand (TypedValue tv))
                   , Delimited Brackets (fromList (map (LLVMOperand . BlockLabel) labs))
                   ]
    LL.Switch tv lab cases ->
      let ty = LL.typedType tv
      in fromItemList [ Item (LLVMOperand (TypedValue tv))
                      , Item (LLVMOperand (BlockLabel lab))
                      , Delimited Parens (fromList (map (LLVMOperand . SwitchTarget ty) cases))
                      ]
    LL.LandingPad ty mtv _ _ ->
      fromList (catMaybes [ Just (LLVMOperand (Type ty))
                , (LLVMOperand . TypedValue) <$> mtv
                ])
    LL.Resume tv -> fromList [ LLVMOperand (TypedValue tv) ]
    LL.Fence mscope ordering ->
      fromList (catMaybes [ (LLVMOperand . ConstantString) <$> mscope
                , Just (LLVMOperand (Ordering ordering))
                ])
    LL.CmpXchg _weak _volatile ptr cmpVal newVal mscope aoSuccess aoFail ->
      fromList $
      catMaybes [ Just (LLVMOperand (TypedValue ptr))
                , Just (LLVMOperand (TypedValue cmpVal))
                , Just (LLVMOperand (TypedValue newVal))
                , (LLVMOperand . ConstantString) <$> mscope
                , Just (LLVMOperand (Ordering aoSuccess))
                , Just (LLVMOperand (Ordering aoFail))
                ]
    LL.AtomicRW _volatile op ptr val mscope ordering ->
      fromList $
      catMaybes [ Just (LLVMOperand (AtomicOp op))
                , Just (LLVMOperand (TypedValue ptr))
                , Just (LLVMOperand (TypedValue val))
                , (LLVMOperand . ConstantString) <$> mscope
                , Just (LLVMOperand (Ordering ordering))
                ]

summarizeModule :: LL.Module -> [(T.Text, T.Text)]
summarizeModule m =
  [ ("Data Layout", T.pack (show (LL.ppDataLayout (LL.modDataLayout m))))
  , ("# Globals", T.pack (show (length (LL.modGlobals m))))
  , ("# Aliases", T.pack (show (length (LL.modAliases m))))
  ]

ppOpcode :: LL.Instr -> T.Text
ppOpcode i =
  case i of
    LL.Ret {} -> "ret"
    LL.RetVoid -> "ret"
    LL.Call False _ _ _ -> "call"
    LL.Call True _ _ _ -> "call tail"
    LL.Invoke {} -> "invoke"
    LL.Alloca {} -> "alloca"
    LL.Load {} -> "load"
    LL.Store {} -> "store"
    LL.ICmp {} -> "icmp"
    LL.FCmp {} -> "fcmp"
    LL.Phi {} -> "phi"
    LL.GEP False _ _ -> "getelementptr"
    LL.GEP True _ _ -> "getelementptr inbounds"
    LL.Select {} -> "select"
    LL.ExtractValue {} -> "extractvalue"
    LL.InsertValue {} -> "insertvalue"
    LL.ExtractElt {} -> "extractelement"
    LL.InsertElt {} -> "insertelement"
    LL.ShuffleVector {} -> "shufflevector"
    LL.Jump {} -> "jump"
    LL.Br {} -> "br"
    LL.Comment {} -> "comment"
    LL.Unreachable {} -> "unreachable"
    LL.Unwind {} -> "unwind"
    LL.VaArg {} -> "va_arg"
    LL.IndirectBr {} -> "indirectbr"
    LL.Switch {} -> "switch"
    LL.LandingPad {} -> "landingpad"
    LL.Resume {} -> "resume"
    LL.Fence {} -> "fence"
    LL.CmpXchg {} -> "cmpxchg"
    LL.AtomicRW {} -> "atomicrw"
    LL.Arith LL.FAdd _ _ -> "fadd"
    LL.Arith LL.FSub _ _ -> "fsub"
    LL.Arith LL.FMul _ _ -> "fmul"
    LL.Arith LL.FDiv _ _ -> "fdiv"
    LL.Arith LL.URem _ _ -> "urem"
    LL.Arith LL.SRem _ _ -> "srem"
    LL.Arith LL.FRem _ _ -> "frem"
    LL.Arith (LL.Add nuw nsw) _ _ -> binOverflow "add" nuw nsw
    LL.Arith (LL.Sub nuw nsw) _ _ -> binOverflow "sub" nuw nsw
    LL.Arith (LL.Mul nuw nsw) _ _ -> binOverflow "mul" nuw nsw
    LL.Arith (LL.UDiv exact) _ _ -> binExact "udiv" exact
    LL.Arith (LL.SDiv exact) _ _ -> binExact "sdiv" exact
    LL.Bit (LL.Shl nuw nsw) _ _ -> binOverflow "shl" nuw nsw
    LL.Bit (LL.Lshr exact) _ _ -> binExact "lshr" exact
    LL.Bit (LL.Ashr exact) _ _ -> binExact "ashr" exact
    LL.Bit LL.And _ _ -> "and"
    LL.Bit LL.Or _ _ -> "or"
    LL.Bit LL.Xor _ _ -> "xor"
    LL.Conv LL.Trunc _ _ -> "trunc"
    LL.Conv LL.ZExt _ _ -> "zext"
    LL.Conv LL.SExt _ _ -> "sext"
    LL.Conv LL.FpTrunc _ _ -> "fptrunc"
    LL.Conv LL.FpExt _ _ -> "fpext"
    LL.Conv LL.FpToUi _ _ -> "fptoui"
    LL.Conv LL.FpToSi _ _ -> "fptosi"
    LL.Conv LL.UiToFp _ _ -> "uitofp"
    LL.Conv LL.SiToFp _ _ -> "uitosp"
    LL.Conv LL.PtrToInt _ _ -> "ptrtoint"
    LL.Conv LL.IntToPtr _ _ -> "inttoptr"
    LL.Conv LL.BitCast _ _ -> "bitcast"

binExact :: String -> Bool -> T.Text
binExact opc exact =
  T.pack (printf "%s%s" opc exact')
  where
    exact' :: String
    exact' = if exact then " exact" else ""

binOverflow :: String -> Bool -> Bool -> T.Text
binOverflow opc nuw nsw =
  T.pack (printf "%s%s%s" opc nuw' nsw')
  where
    nuw' :: String
    nuw' = if nuw then " nuw" else ""
    nsw' :: String
    nsw' = if nsw then " nsw" else ""

defToFunction :: LL.Define -> Maybe (FunctionHandle LLVM s)
defToFunction def = do
  guard (not (null (LL.defBody def)))
  let sym@(LL.Symbol str) = LL.defName def
  return $! FunctionHandle { fhAddress = LLVMAddress (FunctionAddr sym)
                           , fhName = T.pack str
                           }

llvmConfig :: LL.Config
llvmConfig = LL.Config { LL.cfgLoadImplicitType = True
                       , LL.cfgGEPImplicitType = True
                       , LL.cfgUseDILocation = False
                       }

llvmContainingBlocks :: LLVMResult s -> Addr addrTy -> [Block LLVM s]
llvmContainingBlocks lr addr =
  case addr of
    FunctionAddr _ -> []
    BlockAddr (FunctionAddr sym) lab -> fromMaybe [] $ do
      (fh, _, bix) <- M.lookup sym (llvmFunctionIndex lr)
      bb <- M.lookup lab (biLabelToBlock bix)
      return [toBlock fh sym bb]
    InsnAddr (BlockAddr (FunctionAddr sym) lab) _ -> fromMaybe [] $ do
      (fh, _, bix) <- M.lookup sym (llvmFunctionIndex lr)
      bb <- M.lookup lab (biLabelToBlock bix)
      return [toBlock fh sym bb]

llvmFunctionBlocks :: LLVMResult s -> FunctionHandle LLVM s -> [Block LLVM s]
llvmFunctionBlocks lr fh =
  case M.lookup sym (llvmFunctionIndex lr) of
    Nothing -> []
    Just (_, def, _) -> map (toBlock fh sym) (LL.defBody def)
  where
    sym = LL.Symbol (T.unpack (fhName fh))

toBlock :: FunctionHandle LLVM s -> LL.Symbol -> LL.BasicBlock -> Block LLVM s
toBlock fh sym b =
  Block { blockAddress = LLVMAddress (BlockAddr (FunctionAddr sym) lab)
        , blockInstructions = map (toInstruction sym lab) (zip [0..] (LL.bbStmts b))
        , blockFunction = fh
        }
  where
    lab = LL.bbLabel b

toInstruction :: LL.Symbol -> Maybe LL.BlockLabel -> (Int, LL.Stmt) -> (Address LLVM s, Instruction LLVM s)
toInstruction sym lab (idx, stmt) = (LLVMAddress addr, LLVMInstruction stmt)
  where
    addr = InsnAddr (BlockAddr (FunctionAddr sym) lab) idx

