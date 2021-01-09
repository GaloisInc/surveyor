-- | An implementation of 'Architecture' for JVM bytecode
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Surveyor.Core.Architecture.JVM (
  mkInitialJVMResult
  , extendJVMResult
  ) where

import           Control.DeepSeq ( NFData, rnf )
import           Control.Monad ( guard )
import qualified Control.Monad.State.Strict as St
import qualified Control.Once as O
import qualified Data.Foldable as F
import           Data.Int ( Int16, Int32 )
import qualified Data.Map.Strict as M
import           Data.Maybe ( fromMaybe )
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Nonce as NG
import qualified Data.Text as T
import           Data.Word ( Word8, Word16 )
import qualified Lang.Crucible.Backend as CB
import qualified Lang.Crucible.CFG.Core as CCC
import qualified Lang.Crucible.CFG.Extension as CCE
import qualified Lang.Crucible.FunctionHandle as CFH
import qualified Lang.Crucible.JVM as CJ
import qualified Lang.Crucible.Simulator as CS
import qualified Language.JVM.CFG as J
import qualified Language.JVM.Common as J
import qualified Language.JVM.Parser as J
import qualified Prettyprinter as PP

import           Surveyor.Core.Architecture.Class
import qualified Surveyor.Core.Architecture.Crucible as AC
import qualified Surveyor.Core.Architecture.NonceCache as SCAN
import qualified Surveyor.Core.OperandList as OL

data JVM

data JVMResult s =
  JVMResult { jvmNonce :: NG.Nonce s JVM
            , jvmClassIndex :: M.Map J.ClassName (J.Class, M.Map J.MethodKey J.Method)
            , jvmHandleAllocator :: CFH.HandleAllocator
            , jvmContext :: CJ.JVMContext
            }

-- | Construct an empty JVMResult that can be filled in incrementally
mkInitialJVMResult :: NG.Nonce s JVM
                   -> IO (AnalysisResult JVM s)
mkInitialJVMResult nonce = do
  halloc <- CFH.newHandleAllocator
  ctx0 <- CJ.mkInitialJVMContext halloc
  let r0 = JVMResult { jvmNonce = nonce
                     , jvmClassIndex = M.empty
                     , jvmHandleAllocator = halloc
                     , jvmContext = ctx0
                     }
  return AnalysisResult { archResult = JVMAnalysisResult r0
                        , resultIndex = indexResult r0
                        }

-- | Extend a JVMResult with a list of additional classes
extendJVMResult :: AnalysisResult JVM s
                -- ^ The previous analysis result (if any)
                -> [J.Class]
                -- ^ Newly-discovered classes to add to the old result
                -> IO (AnalysisResult JVM s)
extendJVMResult ar0 newClasses = do
  let JVMAnalysisResult r0 = archResult ar0
  let halloc = jvmHandleAllocator r0
  ctx1 <- St.execStateT (mapM_ (CJ.extendJVMContext halloc) newClasses) (jvmContext r0)
  let r1 = r0 { jvmContext = ctx1 }
  let r2 = F.foldl' indexClass r1 newClasses
  return AnalysisResult { archResult = JVMAnalysisResult r2
                        , resultIndex = indexResult r2
                        }
  where
    indexClass r k =
      let midx = F.foldl' indexMethods M.empty (J.classMethods k)
      in r { jvmClassIndex = M.insert (J.className k) (k, midx) (jvmClassIndex r)
           }
    indexMethods idx m = M.insert (J.methodKey m) m idx

indexResult :: JVMResult s -> O.Once (ResultIndex JVM s)
indexResult r = O.once ri
  where
    ri = ResultIndex { riFunctions = jvmFunctions r
                     , riSummary = jvmSummarize r
                     }

data AddrType = ClassK | MethodK | BlockK | InsnK

data Addr addrTy where
  ClassAddr :: !J.ClassName -> Addr 'ClassK
  MethodAddr :: !(Addr 'ClassK) -> !J.MethodKey -> Addr 'MethodK
  BlockAddr :: !(Addr 'MethodK) -> !J.BBId -> Addr 'BlockK
  InsnAddr :: !(Addr 'BlockK) -> !J.PC -> Addr 'InsnK

instance Eq (Addr addrTy) where
  a1 == a2 = isJust (testEquality a1 a2)

instance Ord (Addr addrTy) where
  compare a1 a2 = toOrdering (compareF a1 a2)

deriving instance Show (Addr addrTy)

-- FIXME: This requires a change to the JVM parser to add a real NFData instance
instance NFData (Instruction JVM s) where
  rnf (JVMInstruction i) = i `seq` ()

instance NFData (Operand JVM s) where
  rnf (JVMOperand o) = o `seq` ()

-- | Operands for JVM instructions
--
-- Since the JVM is a stack machine, there aren't many types of operands, and
-- they are all immediates of various types
data JVMOperand' = LocalVariableIndex !J.LocalVariableIndex
                 | Type !J.Type
                 | FieldId !J.FieldId
                 | PC !J.PC
                 | MethodKey !J.MethodKey
                 | SwitchTable [(Int32, J.PC)]
                 | ConstantPoolValue !J.ConstantPoolValue
                 | StringOp String
                 | I16 !Int16
                 | I32 !Int32
                 | W8 !Word8
                 | W16 !Word16

instance IR JVM s where
  data Address JVM s = forall addrTy . JVMAddress (Addr addrTy)
  data Operand JVM s = JVMOperand JVMOperand'
  data Opcode JVM s = JVMOpcode J.Instruction
  data Instruction JVM s = JVMInstruction J.Instruction
  prettyInstruction _ (JVMInstruction i) = PP.viaShow (J.ppInstruction i)
  prettyOpcode (JVMOpcode i) = ppOpcode i
  prettyOperand (JVMAddress _) (JVMOperand op) = ppOperand op
  prettyAddress (JVMAddress a) = ppAddress a
  -- The JVM doesn't have any explicit bound values because they all end up on
  -- the stack
  boundValue _ = Nothing
  opcode (JVMInstruction i) = JVMOpcode i
  operands (JVMInstruction i) = jvmOperands i
  parseAddress _ = Nothing
  rawRepr = Nothing
  showInstructionAddresses _ = False
  operandSelectable (JVMOperand o) =
    case o of
      MethodKey {} -> True
      LocalVariableIndex {} -> True
      -- FIXME: Want to break the switch table down
      SwitchTable {} -> False
      -- Not selectable
      Type {} -> False
      FieldId {} -> False
      PC {} -> False
      ConstantPoolValue {} -> False
      StringOp {} -> False
      I16 {} -> False
      I32 {} -> False
      W8 {} -> False
      W16 {} -> False

type instance CruciblePersonality JVM sym = ()

instance Architecture JVM s where
  data ArchResult JVM s = JVMAnalysisResult (JVMResult s)
  type CrucibleExt JVM = CJ.JVM

  archNonce (AnalysisResult (JVMAnalysisResult r) _) = jvmNonce r
  functions (AnalysisResult _ idx) = riFunctions (O.runOnce idx)

  functionBlocks (AnalysisResult (JVMAnalysisResult r) _) (FunctionHandle (JVMAddress addr) _) =
    case addr of
      maddr@(MethodAddr (ClassAddr klassName) mkey) -> fromMaybe [] $ do
        (_, midx) <- M.lookup klassName (jvmClassIndex r)
        method <- M.lookup mkey midx
        case J.methodBody method of
          J.Code _ _ cfg _ _ _ _ ->
            return [ toBlock maddr method bb | bb <- J.allBBs cfg ]
          J.AbstractMethod -> Nothing
          J.NativeMethod -> Nothing
      _ -> []
  summarizeResult (AnalysisResult _ idx) = riSummary (O.runOnce idx)

  -- TODO
  containingBlocks _ _ = []
  genericSemantics _ _ = Nothing
  alternativeIRs _ = []
  asAlternativeIR _ _ _ = return Nothing
  crucibleCFG = jvmCrucibleCFG
  freshSymbolicEntry _ _ _ = Nothing
  symbolicInitializers = jvmSymbolicInitializers
  fromCrucibleBlock = Nothing

data CrucibleJVMOperand s = CrucibleJVMOperand

instance CrucibleExtension JVM where
  type CrucibleExtensionOperand JVM = CrucibleJVMOperand
  extensionOperandSelectable _ _ = False
  extensionStmtOperands = jvmExtensionStmtOperands
  extensionExprOperands = jvmExtensionExprOperands
  prettyExtensionStmt _ = prettyJVMStmt
  prettyExtensionApp _ = prettyJVMApp
  prettyExtensionOperand _ = prettyJVMExtensionOperand

prettyJVMExtensionOperand :: CrucibleJVMOperand s -> PP.Doc ann
prettyJVMExtensionOperand _ = error "Unimplemented"

prettyJVMApp :: CCE.ExprExtension CJ.JVM (CCC.Reg ctx) tp -> PP.Doc ann
prettyJVMApp = error "Unimplemented"

prettyJVMStmt :: CCE.StmtExtension CJ.JVM (CCC.Reg ctx) tp -> PP.Doc ann
prettyJVMStmt = error "Unimplemented"

jvmExtensionStmtOperands :: SCAN.NonceCache s ctx
                         -> NG.NonceGenerator IO s
                         -> CCE.StmtExtension CJ.JVM (CCC.Reg ctx) tp
                         -> IO [Operand (AC.Crucible JVM) s]
jvmExtensionStmtOperands = error "Unimplemented"

jvmExtensionExprOperands :: SCAN.NonceCache s ctx
                         -> NG.NonceGenerator IO s
                         -> CCE.ExprExtension CJ.JVM (CCC.Reg ctx) tp
                         -> IO [Operand (AC.Crucible JVM) s]
jvmExtensionExprOperands = error "Unimplemented"

jvmCrucibleCFG :: AnalysisResult JVM s
               -> FunctionHandle JVM s
               -> IO (Maybe (CCC.AnyCFG (CrucibleExt JVM)))
jvmCrucibleCFG (AnalysisResult (JVMAnalysisResult jr) _) fh =
  case fhAddress fh of
    JVMAddress (MethodAddr (ClassAddr kls) mk) -> do
      let mmethod = do
            (_, methodMap) <- M.lookup kls (jvmClassIndex jr)
            M.lookup mk methodMap
      case mmethod of
        Nothing -> return Nothing
        Just method -> do
          let verbosity = 0
          let key = (kls, mk)
          case M.lookup key (CJ.methodHandles (jvmContext jr)) of
            Just (CJ.JVMHandleInfo _ hdl) -> do
              CCC.SomeCFG cfg <- CJ.translateMethod (jvmContext jr) verbosity kls method hdl
              return (Just (CCC.AnyCFG cfg))
            _ -> return Nothing
    _ -> return Nothing

jvmSymbolicInitializers :: (CB.IsSymInterface sym)
                        => AnalysisResult JVM s
                        -> sym
                        -> IO ( CS.IntrinsicTypes sym
                              , CFH.HandleAllocator
                              , CS.FunctionBindings () sym (CrucibleExt JVM)
                              , CS.ExtensionImpl () sym (CrucibleExt JVM)
                              , ()
                              )
jvmSymbolicInitializers (AnalysisResult (JVMAnalysisResult jr) _) _sym = do
  let intrinsics = CJ.jvmIntrinsicTypes
  let funcBindings = CFH.emptyHandleMap
  let extImpl = CJ.jvmExtensionImpl
  return (intrinsics, jvmHandleAllocator jr, funcBindings, extImpl, ())

jvmFunctions :: JVMResult s -> [FunctionHandle JVM s]
jvmFunctions r =
  [ FunctionHandle (JVMAddress methodAddr) name
  | (klass, methods) <- M.elems (jvmClassIndex r)
  , let classAddr = ClassAddr (J.className klass)
  , m <- M.elems methods
  , let methodAddr = MethodAddr classAddr (J.methodKey m)
  , let name = T.pack (J.methodName m)
  ]

jvmSummarize :: JVMResult s -> [(T.Text, T.Text)]
jvmSummarize jr =
  [ (T.pack "Classes", t (M.size (jvmClassIndex jr)))
  , (T.pack "Methods", t (sum (map (M.size . snd) (M.elems (jvmClassIndex jr)))))
  ]
  where
    t = T.pack . show

jvmOperands :: J.Instruction -> OL.OperandList (Operand JVM s)
jvmOperands i =
  case i of
    J.Aaload -> OL.fromList []
    J.Aastore -> OL.fromList []
    J.Aconst_null -> OL.fromList []
    J.Aload ix -> OL.fromList [JVMOperand (LocalVariableIndex ix)]
    J.Areturn -> OL.fromList []
    J.Arraylength -> OL.fromList []
    J.Astore ix -> OL.fromList [JVMOperand (LocalVariableIndex ix)]
    J.Athrow -> OL.fromList []
    J.Baload -> OL.fromList []
    J.Bastore -> OL.fromList []
    J.Caload -> OL.fromList []
    J.Castore -> OL.fromList []
    J.Checkcast t -> OL.fromList [JVMOperand (Type t)]
    J.D2f -> OL.fromList []
    J.D2i -> OL.fromList []
    J.D2l -> OL.fromList []
    J.Dadd -> OL.fromList []
    J.Daload -> OL.fromList []
    J.Dastore -> OL.fromList []
    J.Dcmpg -> OL.fromList []
    J.Dcmpl -> OL.fromList []
    J.Ddiv -> OL.fromList []
    J.Dload ix -> OL.fromList [JVMOperand (LocalVariableIndex ix)]
    J.Dmul -> OL.fromList []
    J.Dneg -> OL.fromList []
    J.Drem -> OL.fromList []
    J.Dreturn -> OL.fromList []
    J.Dstore ix -> OL.fromList [JVMOperand (LocalVariableIndex ix)]
    J.Dsub -> OL.fromList []
    J.Dup -> OL.fromList []
    J.Dup_x1 -> OL.fromList []
    J.Dup_x2 -> OL.fromList []
    J.Dup2 -> OL.fromList []
    J.Dup2_x1 -> OL.fromList []
    J.Dup2_x2 -> OL.fromList []
    J.F2d -> OL.fromList []
    J.F2i -> OL.fromList []
    J.F2l -> OL.fromList []
    J.Fadd -> OL.fromList []
    J.Faload -> OL.fromList []
    J.Fastore -> OL.fromList []
    J.Fcmpg -> OL.fromList []
    J.Fcmpl -> OL.fromList []
    J.Fdiv -> OL.fromList []
    J.Fload ix -> OL.fromList [JVMOperand (LocalVariableIndex ix)]
    J.Fmul -> OL.fromList []
    J.Fneg -> OL.fromList []
    J.Frem -> OL.fromList []
    J.Freturn -> OL.fromList []
    J.Fstore ix -> OL.fromList [JVMOperand (LocalVariableIndex ix)]
    J.Fsub -> OL.fromList []
    J.Getfield fid -> OL.fromList [JVMOperand (FieldId fid)]
    J.Getstatic fid -> OL.fromList [JVMOperand (FieldId fid)]
    J.Goto pc -> OL.fromList [JVMOperand (PC pc)]
    J.I2b -> OL.fromList []
    J.I2c -> OL.fromList []
    J.I2d -> OL.fromList []
    J.I2f -> OL.fromList []
    J.I2l -> OL.fromList []
    J.I2s -> OL.fromList []
    J.Iadd -> OL.fromList []
    J.Iaload -> OL.fromList []
    J.Iand -> OL.fromList []
    J.Iastore -> OL.fromList []
    J.Idiv -> OL.fromList []
    J.If_acmpeq pc -> OL.fromList [JVMOperand (PC pc)]
    J.If_acmpne pc -> OL.fromList [JVMOperand (PC pc)]
    J.If_icmpeq pc -> OL.fromList [JVMOperand (PC pc)]
    J.If_icmpne pc -> OL.fromList [JVMOperand (PC pc)]
    J.If_icmplt pc -> OL.fromList [JVMOperand (PC pc)]
    J.If_icmpge pc -> OL.fromList [JVMOperand (PC pc)]
    J.If_icmpgt pc -> OL.fromList [JVMOperand (PC pc)]
    J.If_icmple pc -> OL.fromList [JVMOperand (PC pc)]
    J.Ifeq pc -> OL.fromList [JVMOperand (PC pc)]
    J.Ifne pc -> OL.fromList [JVMOperand (PC pc)]
    J.Iflt pc -> OL.fromList [JVMOperand (PC pc)]
    J.Ifge pc -> OL.fromList [JVMOperand (PC pc)]
    J.Ifgt pc -> OL.fromList [JVMOperand (PC pc)]
    J.Ifle pc -> OL.fromList [JVMOperand (PC pc)]
    J.Ifnonnull pc -> OL.fromList [JVMOperand (PC pc)]
    J.Ifnull pc -> OL.fromList [JVMOperand (PC pc)]
    J.Iinc ix i16 -> OL.fromList [JVMOperand (LocalVariableIndex ix), JVMOperand (I16 i16)]
    J.Iload ix -> OL.fromList [JVMOperand (LocalVariableIndex ix)]
    J.Imul -> OL.fromList []
    J.Ineg -> OL.fromList []
    J.Instanceof t -> OL.fromList [JVMOperand (Type t)]
    J.Invokeinterface s mk -> OL.fromList [JVMOperand (StringOp (J.unClassName s)), JVMOperand (MethodKey mk)]
    J.Invokespecial t mk -> OL.fromList [JVMOperand (Type t), JVMOperand (MethodKey mk)]
    J.Invokestatic s mk -> OL.fromList [JVMOperand (StringOp (J.unClassName s)), JVMOperand (MethodKey mk)]
    J.Invokevirtual t mk -> OL.fromList [JVMOperand (Type t), JVMOperand (MethodKey mk)]
    J.Invokedynamic w16 -> OL.fromList [JVMOperand (W16 w16)]
    J.Ior -> OL.fromList []
    J.Irem -> OL.fromList []
    J.Ireturn -> OL.fromList []
    J.Ishl -> OL.fromList []
    J.Ishr -> OL.fromList []
    J.Istore ix -> OL.fromList [JVMOperand (LocalVariableIndex ix)]
    J.Isub -> OL.fromList []
    J.Iushr -> OL.fromList []
    J.Ixor -> OL.fromList []
    J.Jsr pc -> OL.fromList [JVMOperand (PC pc)]
    J.L2d -> OL.fromList []
    J.L2f -> OL.fromList []
    J.L2i -> OL.fromList []
    J.Ladd -> OL.fromList []
    J.Laload -> OL.fromList []
    J.Land -> OL.fromList []
    J.Lastore -> OL.fromList []
    J.Lcmp -> OL.fromList []
    J.Ldc cpv -> OL.fromList [JVMOperand (ConstantPoolValue cpv)]
    J.Ldiv -> OL.fromList []
    J.Lload ix -> OL.fromList [JVMOperand (LocalVariableIndex ix)]
    J.Lmul -> OL.fromList []
    J.Lneg -> OL.fromList []
    J.Lookupswitch pc tbl -> OL.fromList [JVMOperand (PC pc), JVMOperand (SwitchTable tbl)]
    J.Lor -> OL.fromList []
    J.Lrem -> OL.fromList []
    J.Lreturn -> OL.fromList []
    J.Lshl -> OL.fromList []
    J.Lshr -> OL.fromList []
    J.Lstore ix -> OL.fromList [JVMOperand (LocalVariableIndex ix)]
    J.Lsub -> OL.fromList []
    J.Lushr -> OL.fromList []
    J.Lxor -> OL.fromList []
    J.Monitorenter -> OL.fromList []
    J.Monitorexit -> OL.fromList []
    J.Multianewarray t w8 -> OL.fromList [JVMOperand (Type t), JVMOperand (W8 w8)]
    J.New s -> OL.fromList [JVMOperand (StringOp (J.unClassName s))]
    J.Newarray t -> OL.fromList [JVMOperand (Type t)]
    J.Nop -> OL.fromList []
    J.Pop -> OL.fromList []
    J.Pop2 -> OL.fromList []
    J.Putfield fid -> OL.fromList [JVMOperand (FieldId fid)]
    J.Putstatic fid -> OL.fromList [JVMOperand (FieldId fid)]
    J.Ret ix -> OL.fromList [JVMOperand (LocalVariableIndex ix)]
    J.Return -> OL.fromList []
    J.Saload -> OL.fromList []
    J.Sastore -> OL.fromList []
    J.Swap -> OL.fromList []
    J.Tableswitch pc i32_1 i32_2 pcs ->
      OL.fromItemList [ OL.Item (JVMOperand (PC pc))
                   , OL.Item (JVMOperand (I32 i32_1))
                   , OL.Item (JVMOperand (I32 i32_2))
                   , OL.Delimited OL.Parens (OL.fromList (map (JVMOperand . PC) pcs))
                   ]

toBlock :: Addr 'MethodK -> J.Method -> J.BasicBlock -> Block JVM s
toBlock maddr method bb =
  Block { blockAddress = JVMAddress baddr
        , blockInstructions = map (toInstruction baddr) (J.bbInsts bb)
        , blockFunction = FunctionHandle (JVMAddress maddr) (T.pack (J.methodName method))
        }
  where
    baddr = BlockAddr maddr (J.bbId bb)

toInstruction :: Addr 'BlockK -> (J.PC, J.Instruction) -> (Address JVM s, Instruction JVM s)
toInstruction baddr (pc, i) = (JVMAddress (InsnAddr baddr pc), JVMInstruction i)

ppAddress :: Addr addrTy -> PP.Doc ann
ppAddress a =
  case a of
    ClassAddr s -> PP.pretty (J.unClassName s)
    MethodAddr _ k -> PP.viaShow (J.ppMethodKey k)
    BlockAddr m i -> ppAddress m <> PP.pretty ":" <> PP.viaShow i
    InsnAddr b pc -> ppAddress b <> PP.pretty ":" <> PP.viaShow pc

ppOperand :: JVMOperand' -> PP.Doc ann
ppOperand op =
  case op of
    LocalVariableIndex ix -> PP.viaShow ix
    Type t -> PP.viaShow (J.ppType t)
    FieldId fid -> PP.pretty (J.ppFldId fid)
    PC pc -> PP.viaShow pc
    MethodKey k -> PP.viaShow (J.ppMethodKey k)
    SwitchTable entries ->
      PP.hsep $ PP.punctuate PP.comma [ PP.parens (PP.pretty v <> PP.pretty ", " <> PP.pretty pc)
                                      | (v, pc) <- entries
                                      ]
    ConstantPoolValue cpv -> PP.viaShow cpv
    StringOp s -> PP.dquotes (PP.pretty s)
    I16 i -> PP.viaShow i
    I32 i -> PP.viaShow i
    W8 w -> PP.viaShow w
    W16 w -> PP.viaShow w

ppOpcode :: J.Instruction -> PP.Doc ann
ppOpcode i =
  case i of
    J.Aaload -> PP.pretty "aaload"
    J.Aastore -> PP.pretty "aastore"
    J.Aconst_null -> PP.pretty "aconst_null"
    J.Aload {} -> PP.pretty "aload"
    J.Areturn -> PP.pretty "areturn"
    J.Arraylength -> PP.pretty "arraylength"
    J.Astore {} -> PP.pretty "astore"
    J.Athrow -> PP.pretty "athrow"
    J.Baload -> PP.pretty "baload"
    J.Bastore -> PP.pretty "bastore"
    J.Caload -> PP.pretty "caload"
    J.Castore -> PP.pretty "castore"
    J.Checkcast {} -> PP.pretty "checkcast"
    J.D2f -> PP.pretty "d2f"
    J.D2i -> PP.pretty "d2i"
    J.D2l -> PP.pretty "d2l"
    J.Dadd -> PP.pretty "dadd"
    J.Daload -> PP.pretty "daload"
    J.Dastore -> PP.pretty "dastore"
    J.Dcmpg -> PP.pretty "dcmpg"
    J.Dcmpl -> PP.pretty "dcmpl"
    J.Ddiv -> PP.pretty "ddiv"
    J.Dload {} -> PP.pretty "dload"
    J.Dmul -> PP.pretty "dmul"
    J.Dneg -> PP.pretty "dneg"
    J.Drem -> PP.pretty "drem"
    J.Dreturn -> PP.pretty "dreturn"
    J.Dstore {} -> PP.pretty "dstore"
    J.Dsub -> PP.pretty "dsub"
    J.Dup -> PP.pretty "dup"
    J.Dup_x1 -> PP.pretty "dup_x1"
    J.Dup_x2 -> PP.pretty "dup_x2"
    J.Dup2 -> PP.pretty "dup2"
    J.Dup2_x1 -> PP.pretty "dup2_x1"
    J.Dup2_x2 -> PP.pretty "dup2_x2"
    J.F2d -> PP.pretty "f2d"
    J.F2i -> PP.pretty "f2i"
    J.F2l -> PP.pretty "f2l"
    J.Fadd -> PP.pretty "fadd"
    J.Faload -> PP.pretty "faload"
    J.Fastore -> PP.pretty "fastore"
    J.Fcmpg -> PP.pretty "fcmpg"
    J.Fcmpl -> PP.pretty "fcmpl"
    J.Fdiv -> PP.pretty "fdiv"
    J.Fload {} -> PP.pretty "fload"
    J.Fmul -> PP.pretty "fmul"
    J.Fneg -> PP.pretty "fneg"
    J.Frem -> PP.pretty "frem"
    J.Freturn -> PP.pretty "freturn"
    J.Fstore {} -> PP.pretty "fstore"
    J.Fsub -> PP.pretty "fsub"
    J.Getfield {} -> PP.pretty "getfield"
    J.Getstatic {} -> PP.pretty "getstatic"
    J.Goto {} -> PP.pretty "goto"
    J.I2b -> PP.pretty "i2b"
    J.I2c -> PP.pretty "i2c"
    J.I2d -> PP.pretty "i2d"
    J.I2f -> PP.pretty "i2f"
    J.I2l -> PP.pretty "i2l"
    J.I2s -> PP.pretty "i2s"
    J.Iadd -> PP.pretty "iadd"
    J.Iaload -> PP.pretty "iaload"
    J.Iand -> PP.pretty "iand"
    J.Iastore -> PP.pretty "iastore"
    J.Idiv -> PP.pretty "idiv"
    J.If_acmpeq {} -> PP.pretty "if_acmpeq"
    J.If_acmpne {} -> PP.pretty "if_acmpne"
    J.If_icmpeq {} -> PP.pretty "if_icmpeq"
    J.If_icmpne {} -> PP.pretty "if_icmpne"
    J.If_icmplt {} -> PP.pretty "if_icmplt"
    J.If_icmpge {} -> PP.pretty "if_icmpge"
    J.If_icmpgt {} -> PP.pretty "if_icmpgt"
    J.If_icmple {} -> PP.pretty "if_icmple"
    J.Ifeq {} -> PP.pretty "ifeq"
    J.Ifne {} -> PP.pretty "ifne"
    J.Iflt {} -> PP.pretty "iflt"
    J.Ifge {} -> PP.pretty "ifge"
    J.Ifgt {} -> PP.pretty "ifgt"
    J.Ifle {} -> PP.pretty "ifle"
    J.Ifnonnull {} -> PP.pretty "ifnonnull"
    J.Ifnull {} -> PP.pretty "ifnull"
    J.Iinc {} -> PP.pretty "iinc"
    J.Iload {} -> PP.pretty "iload"
    J.Imul -> PP.pretty "imul"
    J.Ineg -> PP.pretty "ineg"
    J.Instanceof {} -> PP.pretty "instanceof"
    J.Invokeinterface {} -> PP.pretty "invokeinterface"
    J.Invokespecial {} -> PP.pretty "invokespecial"
    J.Invokestatic {} -> PP.pretty "invokestatic"
    J.Invokevirtual {} -> PP.pretty "invokevirtual"
    J.Invokedynamic {} -> PP.pretty "invokedynamic"
    J.Ior -> PP.pretty "ior"
    J.Irem -> PP.pretty "irem"
    J.Ireturn -> PP.pretty "ireturn"
    J.Ishl -> PP.pretty "ishl"
    J.Ishr -> PP.pretty "ishr"
    J.Istore {} -> PP.pretty "istore"
    J.Isub -> PP.pretty "isub"
    J.Iushr -> PP.pretty "iushr"
    J.Ixor -> PP.pretty "ixor"
    J.Jsr {} -> PP.pretty "jsr"
    J.L2d -> PP.pretty "l2d"
    J.L2f -> PP.pretty "l2f"
    J.L2i -> PP.pretty "l2i"
    J.Ladd -> PP.pretty "ladd"
    J.Laload -> PP.pretty "laload"
    J.Land -> PP.pretty "land"
    J.Lastore -> PP.pretty "lastore"
    J.Lcmp -> PP.pretty "lcmp"
    J.Ldc {} -> PP.pretty "ldc"
    J.Ldiv -> PP.pretty "ldiv"
    J.Lload {} -> PP.pretty "lload"
    J.Lmul -> PP.pretty "lmul"
    J.Lneg -> PP.pretty "lneg"
    J.Lookupswitch {} -> PP.pretty "lookupswitch"
    J.Lor -> PP.pretty "lor"
    J.Lrem -> PP.pretty "lrem"
    J.Lreturn -> PP.pretty "lreturn"
    J.Lshl -> PP.pretty "lshl"
    J.Lshr -> PP.pretty "lshr"
    J.Lstore {} -> PP.pretty "lstore"
    J.Lsub -> PP.pretty "lsub"
    J.Lushr -> PP.pretty "lushr"
    J.Lxor -> PP.pretty "lxor"
    J.Monitorenter -> PP.pretty "monitorenter"
    J.Monitorexit -> PP.pretty "monitorexit"
    J.Multianewarray {} -> PP.pretty "multianewarray"
    J.New {} -> PP.pretty "new"
    J.Newarray {} -> PP.pretty "newarray"
    J.Nop -> PP.pretty "nop"
    J.Pop -> PP.pretty "pop"
    J.Pop2 -> PP.pretty "pop2"
    J.Putfield {} -> PP.pretty "putfield"
    J.Putstatic {} -> PP.pretty "putstatic"
    J.Ret {} -> PP.pretty "ret"
    J.Return -> PP.pretty "return"
    J.Saload -> PP.pretty "saload"
    J.Sastore -> PP.pretty "sastore"
    J.Swap -> PP.pretty "swap"
    J.Tableswitch {} -> PP.pretty "tableswitch"

instance Eq (Address JVM s) where
  JVMAddress a1 == JVMAddress a2 = isJust (testEquality a1 a2)

instance Ord (Address JVM s) where
  compare (JVMAddress a1) (JVMAddress a2) = toOrdering (compareF a1 a2)

instance Show (Address JVM s) where
  show (JVMAddress a) = show a

instance NFData (Address JVM s) where
  rnf (JVMAddress a) = a `seq` ()

instance TestEquality Addr where
  testEquality a1 a2 =
    case a1 of
      ClassAddr s1 ->
        case a2 of
          ClassAddr s2 -> do
            guard (s1 == s2)
            return Refl
          _ -> Nothing
      MethodAddr c1 k1 ->
        case a2 of
          MethodAddr c2 k2 -> do
            Refl <- testEquality c1 c2
            guard (k1 == k2)
            return Refl
          _ -> Nothing
      BlockAddr m1 i1 ->
        case a2 of
          BlockAddr m2 i2 -> do
            Refl <- testEquality m1 m2
            guard (i1 == i2)
            return Refl
          _ -> Nothing
      InsnAddr b1 p1 ->
        case a2 of
          InsnAddr b2 p2 -> do
            Refl <- testEquality b1 b2
            guard (p1 == p2)
            return Refl
          _ -> Nothing

instance OrdF Addr where
  compareF a1 a2 =
    case a1 of
      ClassAddr s1 ->
        case a2 of
          ClassAddr s2 -> fromOrdering (compare s1 s2)
          MethodAddr {} -> GTF
          BlockAddr {} -> GTF
          InsnAddr {} -> GTF
      MethodAddr c1 k1 ->
        case a2 of
          ClassAddr {} -> LTF
          MethodAddr c2 k2 ->
            case compareF c1 c2 of
              EQF -> fromOrdering (compare k1 k2)
              LTF -> LTF
              GTF -> GTF
          BlockAddr {} -> GTF
          InsnAddr {} -> GTF
      BlockAddr m1 i1 ->
        case a2 of
          ClassAddr {} -> LTF
          MethodAddr {} -> LTF
          BlockAddr m2 i2 ->
            case compareF m1 m2 of
              EQF -> fromOrdering (compare i1 i2)
              GTF -> GTF
              LTF -> LTF
          InsnAddr {} -> GTF
      InsnAddr b1 p1 ->
        case a2 of
          ClassAddr {} -> LTF
          MethodAddr {} -> LTF
          BlockAddr {} -> LTF
          InsnAddr b2 p2 ->
            case compareF b1 b2 of
              EQF -> fromOrdering (compare p1 p2)
              GTF -> GTF
              LTF -> LTF
