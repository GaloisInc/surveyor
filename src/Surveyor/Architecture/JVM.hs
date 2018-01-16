-- | An implementation of 'Architecture' for JVM bytecode
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
module Surveyor.Architecture.JVM ( mkJVMResult ) where

import           Control.Monad ( guard )
import qualified Data.Foldable as F
import           Data.Int ( Int16, Int32 )
import qualified Data.Map.Strict as M
import           Data.Maybe ( isJust, fromMaybe, mapMaybe )
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Nonce as NG
import qualified Data.Text as T
import           Data.Word ( Word8 )
import qualified Language.JVM.CFG as J
import qualified Language.JVM.Common as J
import qualified Language.JVM.JarReader as J
import qualified Language.JVM.Parser as J
import qualified Text.PrettyPrint as PP
import           Text.Printf ( printf )

import           Surveyor.Architecture.Class

data JVM

data JVMResult s =
  JVMResult { jvmNonce :: NG.Nonce s JVM
            , jvmJarReader :: J.JarReader
            }

mkJVMResult :: NG.Nonce s JVM -> J.JarReader -> SomeResult s
mkJVMResult nonce jr =
  SomeResult (JVMAnalysisResult r)
  where
    r = JVMResult { jvmNonce = nonce
                  , jvmJarReader = jr
                  }

data AddrType = ClassK | MethodK | BlockK | InsnK

data Addr addrTy where
  ClassAddr :: String -> Addr 'ClassK
  MethodAddr :: !(Addr 'ClassK) -> !J.MethodKey -> Addr 'MethodK
  BlockAddr :: !(Addr 'MethodK) -> !J.BBId -> Addr 'BlockK
  InsnAddr :: !(Addr 'BlockK) -> !J.PC -> Addr 'InsnK

instance Eq (Addr addrTy) where
  a1 == a2 = isJust (testEquality a1 a2)

instance Ord (Addr addrTy) where
  compare a1 a2 = toOrdering (compareF a1 a2)

deriving instance Show (Addr addrTy)

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

instance Architecture JVM s where
  data AnalysisResult JVM s = JVMAnalysisResult (JVMResult s)
  data Address JVM s = forall addrTy . JVMAddress (Addr addrTy)
  data Operand JVM s = JVMOperand JVMOperand'
  data Opcode JVM s = JVMOpcode J.Instruction
  data Instruction JVM s = JVMInstruction J.Instruction

  archNonce (JVMAnalysisResult r) = jvmNonce r
  prettyInstruction _ (JVMInstruction i) = T.pack (show (J.ppInstruction i))
  prettyOpcode (JVMOpcode i) = ppOpcode i
  prettyOperand (JVMAddress _) (JVMOperand op) = ppOperand op
  prettyAddress (JVMAddress a) = ppAddress a
  -- The JVM doesn't have any explicit bound values because they all end up on
  -- the stack
  boundValue _ = Nothing
  opcode (JVMInstruction i) = JVMOpcode i

  -- TODO
  summarizeResult _ = []
  genericSemantics _ _ = Nothing
  parseAddress _ = Nothing

ppAddress :: Addr addrTy -> T.Text
ppAddress a =
  case a of
    ClassAddr s -> T.pack s
    MethodAddr _ k -> T.pack (show (J.ppMethodKey k))
    BlockAddr m i -> T.pack (printf "%s:%s" (ppAddress m) (show i))
    InsnAddr b pc -> T.pack (printf "%s:%s" (ppAddress b) (show pc))

ppOperand :: JVMOperand' -> T.Text
ppOperand op =
  case op of
    LocalVariableIndex ix -> T.pack (show ix)
    Type t -> T.pack (show (J.ppType t))
    FieldId fid -> T.pack (J.ppFldId fid)
    PC pc -> T.pack (show pc)
    MethodKey k -> T.pack (show (J.ppMethodKey k))
    SwitchTable entries ->
      T.intercalate ", " [ T.pack (printf "(%d, %d)" v pc) | (v, pc) <- entries ]
    ConstantPoolValue cpv -> T.pack (show cpv)
    StringOp s -> T.pack (show s) -- for quoting
    I16 i -> T.pack (show i)
    I32 i -> T.pack (show i)
    W8 w -> T.pack (show w)

ppOpcode :: J.Instruction -> T.Text
ppOpcode i =
  case i of
    J.Aaload -> "aaload"
    J.Aastore -> "aastore"
    J.Aconst_null -> "aconst_null"
    J.Aload {} -> "aload"
    J.Areturn -> "areturn"
    J.Arraylength -> "arraylength"
    J.Astore {} -> "astore"
    J.Athrow -> "athrow"
    J.Baload -> "baload"
    J.Bastore -> "bastore"
    J.Caload -> "caload"
    J.Castore -> "castore"
    J.Checkcast {} -> "checkcast"
    J.D2f -> "d2f"
    J.D2i -> "d2i"
    J.D2l -> "d2l"
    J.Dadd -> "dadd"
    J.Daload -> "daload"
    J.Dastore -> "dastore"
    J.Dcmpg -> "dcmpg"
    J.Dcmpl -> "dcmpl"
    J.Ddiv -> "ddiv"
    J.Dload {} -> "dload"
    J.Dmul -> "dmul"
    J.Dneg -> "dneg"
    J.Drem -> "drem"
    J.Dreturn -> "dreturn"
    J.Dstore {} -> "dstore"
    J.Dsub -> "dsub"
    J.Dup -> "dup"
    J.Dup_x1 -> "dup_x1"
    J.Dup_x2 -> "dup_x2"
    J.Dup2 -> "dup2"
    J.Dup2_x1 -> "dup2_x1"
    J.Dup2_x2 -> "dup2_x2"
    J.F2d -> "f2d"
    J.F2i -> "f2i"
    J.F2l -> "f2l"
    J.Fadd -> "fadd"
    J.Faload -> "faload"
    J.Fastore -> "fastore"
    J.Fcmpg -> "fcmpg"
    J.Fcmpl -> "fcmpl"
    J.Fdiv -> "fdiv"
    J.Fload {} -> "fload"
    J.Fmul -> "fmul"
    J.Fneg -> "fneg"
    J.Frem -> "frem"
    J.Freturn -> "freturn"
    J.Fstore {} -> "fstore"
    J.Fsub -> "fsub"
    J.Getfield {} -> "getfield"
    J.Getstatic {} -> "getstatic"
    J.Goto {} -> "goto"
    J.I2b -> "i2b"
    J.I2c -> "i2c"
    J.I2d -> "i2d"
    J.I2f -> "i2f"
    J.I2l -> "i2l"
    J.I2s -> "i2s"
    J.Iadd -> "iadd"
    J.Iaload -> "iaload"
    J.Iand -> "iand"
    J.Iastore -> "iastore"
    J.Idiv -> "idiv"
    J.If_acmpeq {} -> "if_acmpeq"
    J.If_acmpne {} -> "if_acmpne"
    J.If_icmpeq {} -> "if_icmpeq"
    J.If_icmpne {} -> "if_icmpne"
    J.If_icmplt {} -> "if_icmplt"
    J.If_icmpge {} -> "if_icmpge"
    J.If_icmpgt {} -> "if_icmpgt"
    J.If_icmple {} -> "if_icmple"
    J.Ifeq {} -> "ifeq"
    J.Ifne {} -> "ifne"
    J.Iflt {} -> "iflt"
    J.Ifge {} -> "ifge"
    J.Ifgt {} -> "ifgt"
    J.Ifle {} -> "ifle"
    J.Ifnonnull {} -> "ifnonnull"
    J.Ifnull {} -> "ifnull"
    J.Iinc {} -> "iinc"
    J.Iload {} -> "iload"
    J.Imul -> "imul"
    J.Ineg -> "ineg"
    J.Instanceof {} -> "instanceof"
    J.Invokeinterface {} -> "invokeinterface"
    J.Invokespecial {} -> "invokespecial"
    J.Invokestatic {} -> "invokestatic"
    J.Invokevirtual {} -> "invokevirtual"
    J.Invokedynamic {} -> "invokedynamic"
    J.Ior -> "ior"
    J.Irem -> "irem"
    J.Ireturn -> "ireturn"
    J.Ishl -> "ishl"
    J.Ishr -> "ishr"
    J.Istore {} -> "istore"
    J.Isub -> "isub"
    J.Iushr -> "iushr"
    J.Ixor -> "ixor"
    J.Jsr {} -> "jsr"
    J.L2d -> "l2d"
    J.L2f -> "l2f"
    J.L2i -> "l2i"
    J.Ladd -> "ladd"
    J.Laload -> "laload"
    J.Land -> "land"
    J.Lastore -> "lastore"
    J.Lcmp -> "lcmp"
    J.Ldc {} -> "ldc"
    J.Ldiv -> "ldiv"
    J.Lload {} -> "lload"
    J.Lmul -> "lmul"
    J.Lneg -> "lneg"
    J.Lookupswitch {} -> "lookupswitch"
    J.Lor -> "lor"
    J.Lrem -> "lrem"
    J.Lreturn -> "lreturn"
    J.Lshl -> "lshl"
    J.Lshr -> "lshr"
    J.Lstore {} -> "lstore"
    J.Lsub -> "lsub"
    J.Lushr -> "lushr"
    J.Lxor -> "lxor"
    J.Monitorenter -> "monitorenter"
    J.Monitorexit -> "monitorexit"
    J.Multianewarray {} -> "multianewarray"
    J.New {} -> "new"
    J.Newarray {} -> "newarray"
    J.Nop -> "nop"
    J.Pop -> "pop"
    J.Pop2 -> "pop2"
    J.Putfield {} -> "putfield"
    J.Putstatic {} -> "putstatic"
    J.Ret {} -> "ret"
    J.Return -> "return"
    J.Saload -> "saload"
    J.Sastore -> "sastore"
    J.Swap -> "swap"
    J.Tableswitch {} -> "tableswitch"

instance Eq (Address JVM s) where
  JVMAddress a1 == JVMAddress a2 = isJust (testEquality a1 a2)

instance Ord (Address JVM s) where
  compare (JVMAddress a1) (JVMAddress a2) = toOrdering (compareF a1 a2)

instance Show (Address JVM s) where
  show (JVMAddress a) = show a

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
