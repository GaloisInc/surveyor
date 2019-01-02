-- | An implementation of 'Architecture' for LLVM bitcode
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Surveyor.Core.Architecture.LLVM ( mkLLVMResult ) where

import           Control.DeepSeq ( NFData, rnf )
import qualified Control.Once as O
import           Control.Monad ( guard )
import qualified Data.Foldable as F
import qualified Data.Map.Strict as M
import           Data.Maybe ( catMaybes, isJust, fromMaybe, mapMaybe )
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Nonce as NG
import qualified Data.Text as T
import qualified Text.LLVM as LL
import qualified Text.LLVM.PP as LL
import qualified Text.PrettyPrint as PP
import           Text.Printf ( printf )

import           Surveyor.Core.Architecture.Class

data LLVM

data LLVMResult s =
  LLVMResult { llvmNonce :: NG.Nonce s LLVM
             , llvmModule :: LL.Module
             , llvmFunctionIndex :: FunctionIndex s
             }

type FunctionIndex s = M.Map LL.Symbol (FunctionHandle LLVM s, LL.Define, BlockIndex)
type BlockIndex = M.Map (Maybe LL.BlockLabel) LL.BasicBlock

indexFunctions :: LL.Module -> FunctionIndex s
indexFunctions = F.foldl' indexDefine M.empty . LL.modDefines
  where
    indexDefine m def
      | null (LL.defBody def) = m
      | otherwise  =
        let blockIndex = F.foldl' indexBlock M.empty (LL.defBody def)
            fh = FunctionHandle { fhAddress = LLVMAddress (FunctionAddr (LL.defName def))
                                , fhName = T.pack (show (LL.ppSymbol (LL.defName def)))
                                }
        in M.insert (LL.defName def) (fh, def, blockIndex) m
    indexBlock m b = M.insert (LL.bbLabel b) b m

mkLLVMResult :: NG.Nonce s LLVM -> LL.Module -> SomeResult s LLVM
mkLLVMResult nonce m =
  SomeResult (AnalysisResult (LLVMAnalysisResult lr) (indexResult lr))
  where
    lr = LLVMResult { llvmNonce = nonce
                    , llvmModule = m
                    , llvmFunctionIndex = indexFunctions m
                    }

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
  alternativeIRs _ = []
  asAlternativeIR _ _ _ = return Nothing

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

stmtOperands :: LL.Stmt -> [Operand LLVM s]
stmtOperands stmt =
  case stmt of
    LL.Result _ instr _ -> instrOperands instr
    LL.Effect instr _ -> instrOperands instr

instrOperands :: LL.Instr -> [Operand LLVM s]
instrOperands i =
  case i of
    LL.RetVoid {} -> []
    LL.Ret rv -> [LLVMOperand (TypedValue rv)]
    LL.Arith _ tv v -> [ LLVMOperand (TypedValue tv)
                       , LLVMOperand (Value v)
                       ]
    LL.Bit _ tv v -> [ LLVMOperand (TypedValue tv)
                     , LLVMOperand (Value v)
                     ]
    LL.Conv _ tv ty -> [ LLVMOperand (TypedValue tv)
                       , LLVMOperand (Type ty)
                       ]
    LL.Call _ ty callee args ->
      LLVMOperand (Type ty) : LLVMOperand (Value callee) : map (LLVMOperand . TypedValue) args
    LL.Alloca ty nelts align ->
      concat [ [LLVMOperand (Type ty)]
             , maybe [] ((:[]) . LLVMOperand . TypedValue) nelts
             , maybe [] ((:[]) . LLVMOperand . ConstantInt) align
             ]
    LL.Load tv _ align ->
      concat [ [LLVMOperand (TypedValue tv)]
             , maybe [] ((:[]) . LLVMOperand . ConstantInt) align
             ]
    LL.Store tv1 tv2 ordering align ->
      concat [ [LLVMOperand (TypedValue tv1), LLVMOperand (TypedValue tv2)]
             , maybe [] ((:[]) . LLVMOperand . Ordering) ordering
             , maybe [] ((:[]) . LLVMOperand . ConstantInt) align
             ]
    LL.ICmp _ tv v -> [LLVMOperand (TypedValue tv), LLVMOperand (Value v)]
    LL.FCmp _ tv v -> [LLVMOperand (TypedValue tv), LLVMOperand (Value v)]
    LL.Phi ty vs -> LLVMOperand (Type ty) : map (LLVMOperand . Value . fst) vs
    LL.GEP _ tv tvs -> LLVMOperand (TypedValue tv) : map (LLVMOperand . TypedValue) tvs
    LL.Select tv1 tv2 v -> [ LLVMOperand (TypedValue tv1)
                           , LLVMOperand (TypedValue tv2)
                           , LLVMOperand (Value v)
                           ]
    LL.ExtractValue tv ixs -> LLVMOperand (TypedValue tv) : map (LLVMOperand . ConstantInt . fromIntegral) ixs
    LL.InsertValue tv1 tv2 ixs ->
      LLVMOperand (TypedValue tv1) : LLVMOperand (TypedValue tv2) : map (LLVMOperand . ConstantInt . fromIntegral) ixs
    LL.ExtractElt tv v -> [LLVMOperand (TypedValue tv), LLVMOperand (Value v)]
    LL.InsertElt tv1 tv2 v -> [ LLVMOperand (TypedValue tv1)
                              , LLVMOperand (TypedValue tv2)
                              , LLVMOperand (Value v)
                              ]
    LL.ShuffleVector tv1 v tv2 -> [ LLVMOperand (TypedValue tv1)
                                  , LLVMOperand (Value v)
                                  , LLVMOperand (TypedValue tv2)
                                  ]
    LL.Jump lab -> [ LLVMOperand (BlockLabel lab) ]
    LL.Br tv l1 l2 -> [ LLVMOperand (TypedValue tv)
                      , LLVMOperand (BlockLabel l1)
                      , LLVMOperand (BlockLabel l2)
                      ]
    LL.Invoke ty v tvs l1 l2 ->
      concat [ [LLVMOperand (Type ty), LLVMOperand (Value v)]
             , map (LLVMOperand . TypedValue) tvs
             , [ LLVMOperand (BlockLabel l1), LLVMOperand (BlockLabel l2) ]
             ]
    LL.Comment s -> [ LLVMOperand (ConstantString s) ]
    LL.Unreachable -> []
    LL.Unwind -> []
    LL.VaArg tv t -> [ LLVMOperand (TypedValue tv), LLVMOperand (Type t) ]
    LL.IndirectBr tv labs ->
      LLVMOperand (TypedValue tv) : map (LLVMOperand . BlockLabel) labs
    LL.Switch tv lab cases ->
      let ty = LL.typedType tv
      in LLVMOperand (TypedValue tv) : LLVMOperand (BlockLabel lab) : map (LLVMOperand . SwitchTarget ty) cases
    LL.LandingPad ty mtv _ _ ->
      catMaybes [ Just (LLVMOperand (Type ty))
                , (LLVMOperand . TypedValue) <$> mtv
                ]
    LL.Resume tv -> [ LLVMOperand (TypedValue tv) ]
    LL.Fence mscope ordering ->
      catMaybes [ (LLVMOperand . ConstantString) <$> mscope
                , Just (LLVMOperand (Ordering ordering))
                ]
    LL.CmpXchg _weak _volatile ptr cmpVal newVal mscope aoSuccess aoFail ->
      catMaybes [ Just (LLVMOperand (TypedValue ptr))
                , Just (LLVMOperand (TypedValue cmpVal))
                , Just (LLVMOperand (TypedValue newVal))
                , (LLVMOperand . ConstantString) <$> mscope
                , Just (LLVMOperand (Ordering aoSuccess))
                , Just (LLVMOperand (Ordering aoFail))
                ]
    LL.AtomicRW _volatile op ptr val mscope ordering ->
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
      bb <- M.lookup lab bix
      return [toBlock fh sym bb]
    InsnAddr (BlockAddr (FunctionAddr sym) lab) _ -> fromMaybe [] $ do
      (fh, _, bix) <- M.lookup sym (llvmFunctionIndex lr)
      bb <- M.lookup lab bix
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

