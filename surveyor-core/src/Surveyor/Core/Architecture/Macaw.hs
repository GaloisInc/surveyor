{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Surveyor.Core.Architecture.Macaw (
  Macaw,
  macawForBlocks,
  MacawException(..)
  ) where

import           Control.DeepSeq ( NFData(rnf), deepseq )
import           Control.Lens ( (^.) )
import qualified Control.Exception as X
import qualified Data.Macaw.CFG as MC
import qualified Data.Macaw.Discovery.State as MC
import qualified Data.Macaw.Types as MT
import qualified Data.Map as M
import           Data.Parameterized.Classes ( ShowF, showF )
import qualified Data.Parameterized.List as PL
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.Nonce as PN
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Parameterized.TraversableFC as FC
import           Data.Proxy ( Proxy(..) )
import qualified Data.Text as T
import qualified Data.Vector as V
import           Data.Word ( Word64 )
import qualified Fmt as Fmt
import           Fmt ( (||+), (+|), (|+) )
import qualified Renovate as R
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Text.Printf ( printf )
import           Text.Read ( readMaybe )

import Surveyor.Core.Architecture.Class
import Surveyor.Core.IRRepr ( Macaw )

macawForBlocks :: forall arch s
                . (MC.MemWidth (MC.ArchAddrWidth arch))
               => PN.NonceGenerator IO s
               -> R.BlockInfo arch
               -> R.ConcreteAddress arch
               -> [(MC.ArchSegmentOff arch, Block arch s)]
               -> IO [(Block arch s, Block (Macaw arch) s)]
macawForBlocks nonceGen binfo faddr blocks = do
  case M.lookup faddr (R.biFunctions binfo) of
    Nothing -> X.throwIO (NoMacawFunctionFor faddr)
    Just (_, Some dfi) -> do
      let fh :: FunctionHandle (Macaw arch) s
          fh = FunctionHandle { fhAddress = MacawAddress (R.absoluteAddress faddr)
                              , fhName = T.pack (show faddr)
                              }
      mapM (toMacawBlock nonceGen dfi fh) blocks

toMacawBlock :: forall arch s ids
              . (MC.MemWidth (MC.ArchAddrWidth arch))
             => PN.NonceGenerator IO s
             -> MC.DiscoveryFunInfo arch ids
             -> FunctionHandle (Macaw arch) s
             -> (MC.ArchSegmentOff arch, Block arch s)
             -> IO (Block arch s, Block (Macaw arch) s)
toMacawBlock nonceGen dfi fh (baddr, b) = do
  case M.lookup baddr (dfi ^. MC.parsedBlocks) of
    Nothing -> X.throwIO (NoMacawBlockFor (Proxy @arch) baddr)
    Just pb -> do
      let stmts = MC.blockStatementList pb
      insns <- buildInsnList 0 (MC.stmtsNonterm stmts) (MC.stmtsTerm stmts)
      let mb = Block { blockAddress = BlockNumber (MC.stmtsIdent stmts)
                     , blockInstructions = insns
                     , blockFunction = fh
                     }
      return (b, mb)
    where
      buildInsnList insnIdx nonterms term =
        case nonterms of
          [] -> do
            ops <- macawTermStmtOperands nonceGen term
            return [(InstructionNumber insnIdx, MacawTermStmt term ops)]
          s:stmts -> do
            mbv <- macawBoundValue nonceGen s
            ops <- macawStmtOperands nonceGen s
            rest <- buildInsnList (insnIdx + 1) stmts term
            return ((InstructionNumber insnIdx, MacawStmt s mbv ops) : rest)

-- FIXME: Alter the instruction wrappers to contain lists of operands.  Each
-- operand is built here, eagerly, and we can associate a new nonce from our
-- state thread with each value.  We need to maintain a map from values to our
-- custom nonces in this context, as we cannot let the state parameter of the
-- underlying values escape.

data MacawException where
  NoMacawFunctionFor :: (MC.MemWidth (MC.ArchAddrWidth arch)) => R.ConcreteAddress arch -> MacawException
  NoMacawBlockFor :: (MC.MemWidth (MC.ArchAddrWidth arch)) => Proxy arch -> MC.ArchSegmentOff arch -> MacawException

deriving instance Show MacawException
instance X.Exception MacawException

type MacawConstraints arch s = ( Ord (Address (Macaw arch) s)
                               , Show (Address (Macaw arch) s)
                               , NFData (Address (Macaw arch) s)
                               , NFData (Instruction (Macaw arch) s)
                               , MC.ArchConstraints arch
                               )

instance (MacawConstraints arch s) => IR (Macaw arch) s where
  data Instruction (Macaw arch) s = forall ids . MacawStmt (MC.Stmt arch ids) (Maybe (Operand (Macaw arch) s)) [Operand (Macaw arch) s]
                                  | forall ids . MacawTermStmt (MC.ParsedTermStmt arch ids) [Operand (Macaw arch) s]
  data Address (Macaw arch) s = MacawAddress (MC.ArchAddrWord arch)
                              | BlockNumber Word64
                              | InstructionNumber Int
  data Operand (Macaw arch) s = forall tp . MacawOperand (PN.Nonce s tp) (MacawOperand arch s)
  data Opcode (Macaw arch) s = MacawOpcode (MacawOpcode arch s)

  boundValue (MacawStmt _stmt mbv _) = mbv
  boundValue (MacawTermStmt _ _) = Nothing

  operands (MacawStmt _stmt _ ops) = ops
  operands (MacawTermStmt _stmt ops) = ops
  opcode (MacawStmt stmt _ _) = macawStmtOpcode stmt
  opcode (MacawTermStmt stmt _) = macawTermStmtOpcode stmt
  prettyInstruction addr (MacawStmt stmt _ _) =
    case addr of
      MacawAddress a -> T.pack (show (MC.ppStmt (\off -> PP.text (show (a + off))) stmt))
      BlockNumber _ -> Fmt.fmt ("" +|MC.ppStmt (\off -> PP.text (show off)) stmt||+ "")
      InstructionNumber i -> Fmt.fmt ("i" +|i|+ "")

  prettyInstruction _ (MacawTermStmt stmt _) = T.pack (show stmt)
  prettyAddress a =
    case a of
      MacawAddress addr -> T.pack (show addr)
      BlockNumber n -> Fmt.fmt ("b" +| n ||+"")
      InstructionNumber n -> Fmt.fmt ("i" +| n ||+"")
  prettyOpcode (MacawOpcode opc) = macawPrettyOpcode opc
  prettyOperand _addr (MacawOperand _ opr) =
    macawPrettyOperand opr
  parseAddress t = (MacawAddress . fromIntegral) <$> ((readMaybe t) :: Maybe Word64)

deriving instance Eq (Address (Macaw arch) s)
deriving instance Ord (Address (Macaw arch) s)
instance (MacawConstraints arch s) => Show (Address (Macaw arch) s) where
  show a = T.unpack (prettyAddress a)
instance NFData (Address (Macaw arch) s) where
  rnf a =
    case a of
      MacawAddress !_w -> ()
      BlockNumber !_bn -> ()
      InstructionNumber !_n -> ()

instance NFData (Instruction (Macaw arch) s) where
  rnf i =
    case i of
      MacawStmt !_s binder ops -> binder `deepseq` ops `deepseq` ()
      MacawTermStmt !_ts ops -> ops `deepseq` ()

instance NFData (Operand (Macaw arch) s) where
  rnf (MacawOperand !_nonce mo) = mo `deepseq` ()

macawBoundValue :: PN.NonceGenerator IO s -> MC.Stmt arch ids -> IO (Maybe (Operand (Macaw arch) s))
macawBoundValue ng stmt =
  case stmt of
    MC.AssignStmt asgn -> do
      n1 <- PN.freshNonce ng
      return (Just (MacawOperand n1 (Binder (MC.assignId asgn))))
    _ -> return Nothing

macawStmtOpcode :: MC.Stmt arch ids -> Opcode (Macaw arch) s
macawStmtOpcode stmt =
  case stmt of
    MC.Comment _ -> MacawOpcode Comment
    MC.ArchState {} -> MacawOpcode ArchState
    MC.ExecArchStmt s -> MacawOpcode (ExecArchStmt s)
    MC.AssignStmt asgn ->
      case MC.assignRhs asgn of
        MC.SetUndefined {} -> MacawOpcode Undefined
        MC.EvalApp app -> MacawOpcode (App app)
        MC.ReadMem {} -> MacawOpcode ReadMem
        MC.CondReadMem {} -> MacawOpcode CondReadMem
        MC.EvalArchFn fn _tp -> MacawOpcode (EvalArchFn fn)
    MC.WriteMem {} -> MacawOpcode WriteMem
    MC.InstructionStart {} -> MacawOpcode Comment

macawTermStmtOpcode :: MC.ParsedTermStmt arch ids -> Opcode (Macaw arch) s
macawTermStmtOpcode stmt =
  case stmt of
    MC.ParsedCall _ Nothing -> MacawOpcode TailCall
    MC.ParsedCall _ (Just _) -> MacawOpcode Call
    MC.ParsedJump {} -> MacawOpcode Jump
    MC.ParsedLookupTable {} -> MacawOpcode IndirectJumpTable
    MC.ParsedReturn {} -> MacawOpcode Return
    MC.ParsedIte {} -> MacawOpcode ITE
    MC.ParsedTranslateError t -> MacawOpcode (TranslateError t)
    MC.ClassifyFailure {} -> MacawOpcode ClassifyFailure
    MC.ParsedArchTermStmt t _regs _next -> MacawOpcode (ArchTermStmt t)
    MC.PLTStub _regs _addr _relo -> error "Unhandled PLT stub"

-- FIXME: Nonces need to be memoized (i.e., the same operand appearing in
-- multiple locations should get the same global nonce)
macawTermStmtOperands :: PN.NonceGenerator IO s -> MC.ParsedTermStmt arch ids -> IO [Operand (Macaw arch) s]
macawTermStmtOperands ng stmt =
  case stmt of
    MC.PLTStub regs next relo -> do
      n1 <- PN.freshNonce ng
      n2 <- PN.freshNonce ng
      n3 <- PN.freshNonce ng
      return [ MacawOperand n1 (StateUpdate regs)
             , MacawOperand n2 (SegmentOffset next)
             , MacawOperand n3 (Relocation relo)
             ]
    MC.ParsedCall regs Nothing -> do
      n1 <- PN.freshNonce ng
      return [MacawOperand n1 (RegState regs)]
    MC.ParsedCall regs (Just next) -> do
      n1 <- PN.freshNonce ng
      n2 <- PN.freshNonce ng
      return [MacawOperand n1 (RegState regs), MacawOperand n2 (SegmentOffset next)]
    MC.ParsedJump regs next -> do
      n1 <- PN.freshNonce ng
      n2 <- PN.freshNonce ng
      return [MacawOperand n1 (RegState regs), MacawOperand n2 (SegmentOffset next)]
    MC.ParsedLookupTable regs idx tbl -> do
      n1 <- PN.freshNonce ng
      n2 <- PN.freshNonce ng
      n3 <- PN.freshNonce ng
      return [ MacawOperand n1 (RegState regs)
             , MacawOperand n2 (Value idx)
             , MacawOperand n3 (JumpTable tbl)
             ]
    MC.ParsedReturn regs -> do
      n1 <- PN.freshNonce ng
      return [MacawOperand n1 (RegState regs)]
    MC.ParsedIte c tb eb -> do
      n1 <- PN.freshNonce ng
      n2 <- PN.freshNonce ng
      n3 <- PN.freshNonce ng
      return [ MacawOperand n1 (Value c)
             , MacawOperand n2 (BlockId (MC.stmtsIdent tb))
             , MacawOperand n3 (BlockId (MC.stmtsIdent eb))
             ]
    MC.ParsedArchTermStmt _ regs Nothing -> do
      n1 <- PN.freshNonce ng
      return [MacawOperand n1 (RegState regs)]
    MC.ParsedArchTermStmt _ regs (Just next) -> do
      n1 <- PN.freshNonce ng
      n2 <- PN.freshNonce ng
      return [MacawOperand n1 (RegState regs), MacawOperand n2 (SegmentOffset next)]
    MC.ParsedTranslateError {} -> return []
    MC.ClassifyFailure regs -> do
      n1 <- PN.freshNonce ng
      return [MacawOperand n1 (RegState regs)]

macawStmtOperands :: PN.NonceGenerator IO s -> MC.Stmt arch idss -> IO [Operand (Macaw arch) s]
macawStmtOperands ng stmt =
  case stmt of
    MC.Comment t -> do
      n1 <- PN.freshNonce ng
      return [MacawOperand n1 (CommentText t)]
    MC.InstructionStart addr t -> do
      n1 <- PN.freshNonce ng
      n2 <- PN.freshNonce ng
      return [MacawOperand n1 (AddressLiteral addr), MacawOperand n2 (CommentText t)]
    MC.WriteMem addr memRep v -> do
      n1 <- PN.freshNonce ng
      n2 <- PN.freshNonce ng
      n3 <- PN.freshNonce ng
      return [ MacawOperand n1 (Value addr)
             , MacawOperand n2 (MemRepr memRep)
             , MacawOperand n3 (Value v)
             ]
    MC.ArchState addr upd -> do
      n1 <- PN.freshNonce ng
      n2 <- PN.freshNonce ng
      return [MacawOperand n1 (Addr addr), MacawOperand n2 (StateUpdate upd)]
  --   -- FIXME: Need a helper to deconstruct these on a per-arch basis
    MC.ExecArchStmt {} -> return []
    MC.AssignStmt asgn -> do
      case MC.assignRhs asgn of
        -- FIXME: Need a helper to dissect arch-specific functions
        MC.EvalArchFn {} -> return []
        MC.SetUndefined rep -> do
          n1 <- PN.freshNonce ng
          return [MacawOperand n1 (TypeRepr rep)]
        MC.ReadMem addr memRep -> do
          n1 <- PN.freshNonce ng
          n2 <- PN.freshNonce ng
          return [MacawOperand n1 (Value addr), MacawOperand n2 (MemRepr memRep)]
        MC.CondReadMem memRep c addr def -> do
          n1 <- PN.freshNonce ng
          n2 <- PN.freshNonce ng
          n3 <- PN.freshNonce ng
          n4 <- PN.freshNonce ng
          return [ MacawOperand n1 (MemRepr memRep)
                 , MacawOperand n2 (Value c)
                 , MacawOperand n3 (Value addr)
                 , MacawOperand n4 (Value def)
                 ]
        MC.EvalApp app ->
          case app of
            MC.Eq v1 v2 -> do
              n1 <- PN.freshNonce ng
              n2 <- PN.freshNonce ng
              return [MacawOperand n1 (Value v1), MacawOperand n2 (Value v2)]
            MC.Mux rep c v1 v2 -> do
              n1 <- PN.freshNonce ng
              n2 <- PN.freshNonce ng
              n3 <- PN.freshNonce ng
              n4 <- PN.freshNonce ng
              return [ MacawOperand n1 (TypeRepr rep)
                     , MacawOperand n2 (Value c)
                     , MacawOperand n3 (Value v1)
                     , MacawOperand n4 (Value v2)
                     ]
            MC.TupleField tps v ix -> do
              n1 <- PN.freshNonce ng
              n2 <- PN.freshNonce ng
              n3 <- PN.freshNonce ng
              return [MacawOperand n1 (TypeReprs tps), MacawOperand n2 (Value v), MacawOperand n3 (Index ix)]
            MC.AndApp v1 v2 -> do
              n1 <- PN.freshNonce ng
              n2 <- PN.freshNonce ng
              return [MacawOperand n1 (Value v1), MacawOperand n2 (Value v2)]
            MC.OrApp v1 v2 -> do
              n1 <- PN.freshNonce ng
              n2 <- PN.freshNonce ng
              return [MacawOperand n1 (Value v1), MacawOperand n2 (Value v2)]
            MC.NotApp v -> do
              n1 <- PN.freshNonce ng
              return [MacawOperand n1 (Value v)]
            MC.XorApp v1 v2 -> do
              n1 <- PN.freshNonce ng
              n2 <- PN.freshNonce ng
              return [MacawOperand n1 (Value v1), MacawOperand n2 (Value v2)]
            MC.Trunc v tp -> do
              n1 <- PN.freshNonce ng
              n2 <- PN.freshNonce ng
              return [MacawOperand n1 (Value v), MacawOperand n2 (NatRepr tp)]
            MC.SExt v tp -> do
              n1 <- PN.freshNonce ng
              n2 <- PN.freshNonce ng
              return [MacawOperand n1 (Value v), MacawOperand n2 (NatRepr tp)]
            MC.UExt v tp -> do
              n1 <- PN.freshNonce ng
              n2 <- PN.freshNonce ng
              return [MacawOperand n1 (Value v), MacawOperand n2 (NatRepr tp)]
            MC.BVAdd tp v1 v2 -> do
              n1 <- PN.freshNonce ng
              n2 <- PN.freshNonce ng
              n3 <- PN.freshNonce ng
              return [MacawOperand n1 (NatRepr tp), MacawOperand n2 (Value v1), MacawOperand n3 (Value v2)]
            MC.BVAdc tp v1 v2 c -> do
              n1 <- PN.freshNonce ng
              n2 <- PN.freshNonce ng
              n3 <- PN.freshNonce ng
              n4 <- PN.freshNonce ng
              return [ MacawOperand n1 (NatRepr tp)
                     , MacawOperand n2 (Value v1)
                     , MacawOperand n3 (Value v2)
                     , MacawOperand n4 (Value c)
                     ]
            MC.BVSub tp v1 v2 -> do
              n1 <- PN.freshNonce ng
              n2 <- PN.freshNonce ng
              n3 <- PN.freshNonce ng
              return [MacawOperand n1 (NatRepr tp), MacawOperand n2 (Value v1), MacawOperand n3 (Value v2)]
            MC.BVSbb tp v1 v2 c -> do
              n1 <- PN.freshNonce ng
              n2 <- PN.freshNonce ng
              n3 <- PN.freshNonce ng
              n4 <- PN.freshNonce ng
              return [ MacawOperand n1 (NatRepr tp)
                     , MacawOperand n2 (Value v1)
                     , MacawOperand n3 (Value v2)
                     , MacawOperand n4 (Value c)
                     ]
            MC.BVMul tp v1 v2 -> do
              n1 <- PN.freshNonce ng
              n2 <- PN.freshNonce ng
              n3 <- PN.freshNonce ng
              return [MacawOperand n1 (NatRepr tp), MacawOperand n2 (Value v1), MacawOperand n3 (Value v2)]
            MC.BVUnsignedLe v1 v2 -> do
              n1 <- PN.freshNonce ng
              n2 <- PN.freshNonce ng
              return [MacawOperand n1 (Value v1), MacawOperand n2 (Value v2)]
            MC.BVUnsignedLt v1 v2 -> do
              n1 <- PN.freshNonce ng
              n2 <- PN.freshNonce ng
              return [MacawOperand n1 (Value v1), MacawOperand n2 (Value v2)]
            MC.BVSignedLe v1 v2 -> do
              n1 <- PN.freshNonce ng
              n2 <- PN.freshNonce ng
              return [MacawOperand n1 (Value v1), MacawOperand n2 (Value v2)]
            MC.BVSignedLt v1 v2 -> do
              n1 <- PN.freshNonce ng
              n2 <- PN.freshNonce ng
              return [MacawOperand n1 (Value v1), MacawOperand n2 (Value v2)]
            MC.BVTestBit v1 v2 -> do
              n1 <- PN.freshNonce ng
              n2 <- PN.freshNonce ng
              return [MacawOperand n1 (Value v1), MacawOperand n2 (Value v2)]
            MC.BVComplement tp v -> do
              n1 <- PN.freshNonce ng
              n2 <- PN.freshNonce ng
              return [MacawOperand n1 (NatRepr tp), MacawOperand n2 (Value v)]
            MC.BVAnd tp v1 v2 -> do
              n1 <- PN.freshNonce ng
              n2 <- PN.freshNonce ng
              n3 <- PN.freshNonce ng
              return [MacawOperand n1 (NatRepr tp), MacawOperand n2 (Value v1), MacawOperand n3 (Value v2)]
            MC.BVOr tp v1 v2 -> do
              n1 <- PN.freshNonce ng
              n2 <- PN.freshNonce ng
              n3 <- PN.freshNonce ng
              return [MacawOperand n1 (NatRepr tp), MacawOperand n2 (Value v1), MacawOperand n3 (Value v2)]
            MC.BVXor tp v1 v2 -> do
              n1 <- PN.freshNonce ng
              n2 <- PN.freshNonce ng
              n3 <- PN.freshNonce ng
              return [MacawOperand n1 (NatRepr tp), MacawOperand n2 (Value v1), MacawOperand n3 (Value v2)]
            MC.BVShl tp v1 v2 -> do
              n1 <- PN.freshNonce ng
              n2 <- PN.freshNonce ng
              n3 <- PN.freshNonce ng
              return [MacawOperand n1 (NatRepr tp), MacawOperand n2 (Value v1), MacawOperand n3 (Value v2)]
            MC.BVShr tp v1 v2 -> do
              n1 <- PN.freshNonce ng
              n2 <- PN.freshNonce ng
              n3 <- PN.freshNonce ng
              return [MacawOperand n1 (NatRepr tp), MacawOperand n2 (Value v1), MacawOperand n3 (Value v2)]
            MC.BVSar tp v1 v2 -> do
              n1 <- PN.freshNonce ng
              n2 <- PN.freshNonce ng
              n3 <- PN.freshNonce ng
              return [MacawOperand n1 (NatRepr tp), MacawOperand n2 (Value v1), MacawOperand n3 (Value v2)]
            MC.UadcOverflows v1 v2 c -> do
              n1 <- PN.freshNonce ng
              n2 <- PN.freshNonce ng
              n3 <- PN.freshNonce ng
              return [MacawOperand n1 (Value v1), MacawOperand n2 (Value v2), MacawOperand n3 (Value c)]
            MC.SadcOverflows v1 v2 c -> do
              n1 <- PN.freshNonce ng
              n2 <- PN.freshNonce ng
              n3 <- PN.freshNonce ng
              return [MacawOperand n1 (Value v1), MacawOperand n2 (Value v2), MacawOperand n3 (Value c)]
            MC.UsbbOverflows v1 v2 c -> do
              n1 <- PN.freshNonce ng
              n2 <- PN.freshNonce ng
              n3 <- PN.freshNonce ng
              return [MacawOperand n1 (Value v1), MacawOperand n2 (Value v2), MacawOperand n3 (Value c)]
            MC.SsbbOverflows v1 v2 c -> do
              n1 <- PN.freshNonce ng
              n2 <- PN.freshNonce ng
              n3 <- PN.freshNonce ng
              return [MacawOperand n1 (Value v1), MacawOperand n2 (Value v2), MacawOperand n3 (Value c)]
            MC.PopCount tp v -> do
              n1 <- PN.freshNonce ng
              n2 <- PN.freshNonce ng
              return [MacawOperand n1 (NatRepr tp), MacawOperand n2 (Value v)]
            MC.ReverseBytes tp v -> do
              n1 <- PN.freshNonce ng
              n2 <- PN.freshNonce ng
              return [MacawOperand n1 (NatRepr tp), MacawOperand n2 (Value v)]
            MC.Bsf tp v -> do
              n1 <- PN.freshNonce ng
              n2 <- PN.freshNonce ng
              return [MacawOperand n1 (NatRepr tp), MacawOperand n2 (Value v)]
            MC.Bsr tp v -> do
              n1 <- PN.freshNonce ng
              n2 <- PN.freshNonce ng
              return [MacawOperand n1 (NatRepr tp), MacawOperand n2 (Value v)]

macawPrettyOpcode :: (MacawConstraints arch s) => MacawOpcode arch s -> T.Text
macawPrettyOpcode opc =
  case opc of
    Comment -> T.pack "#"
    WriteMem -> T.pack "write_mem"
    Undefined -> T.pack "undefined"
    ReadMem -> T.pack "read_mem"
    CondReadMem -> T.pack "cond_read_mem"
    ArchState -> T.pack "arch_state"
    Call -> T.pack "call"
    TailCall -> T.pack "tailcall"
    Jump -> T.pack "jump"
    IndirectJumpTable -> T.pack "indirect-jump"
    ClassifyFailure -> T.pack "classify-failure"
    ITE -> T.pack "ite"
    Return -> T.pack "return"
    TranslateError t -> T.pack (printf "translate-error %s" t)
    -- FIXME: This isn't very good - we need an arch-specific backend to print
    -- each archfn correctly
    EvalArchFn fn -> T.pack (show (MC.ppArchFn (Just . MC.ppValue 0) fn))
    -- FIXME: Same
    ExecArchStmt stmt -> T.pack (show (MC.ppArchStmt (MC.ppValue 0) stmt))
    -- FIXME: Same
    ArchTermStmt t -> T.pack (show (MC.prettyF t))
    App a ->
      case a of
        MC.Eq {} -> T.pack "eq"
        MC.Mux {} -> T.pack "mux"
        MC.TupleField {} -> T.pack "tuple"
        MC.AndApp {} -> T.pack "andp"
        MC.OrApp {} -> T.pack "orp"
        MC.NotApp {} -> T.pack "notp"
        MC.XorApp {} -> T.pack "xorp"
        MC.Trunc {} -> T.pack "trunc"
        MC.SExt {} -> T.pack "sext"
        MC.UExt {} -> T.pack "uext"
        MC.BVAdd {} -> T.pack "bv_add"
        MC.BVAdc {} -> T.pack "bv_adc"
        MC.BVSub {} -> T.pack "bv_sub"
        MC.BVSbb {} -> T.pack "bv_sbb"
        MC.BVMul {} -> T.pack "bv_mul"
        MC.BVUnsignedLe {} -> T.pack "bv_ule"
        MC.BVUnsignedLt {} -> T.pack "bv_ult"
        MC.BVSignedLe {} -> T.pack "bv_sle"
        MC.BVSignedLt {} -> T.pack "bv_slt"
        MC.BVTestBit {} -> T.pack "test_bit"
        MC.BVComplement {} -> T.pack "bv_complement"
        MC.BVAnd {} -> T.pack "bv_and"
        MC.BVOr {} -> T.pack "bv_or"
        MC.BVXor {} -> T.pack "bv_xor"
        MC.BVShl {} -> T.pack "bv_shl"
        MC.BVShr {} -> T.pack "bv_shr"
        MC.BVSar {} -> T.pack "bv_sar"
        MC.UadcOverflows {} -> T.pack "uadc_overflows"
        MC.SadcOverflows {} -> T.pack "sadc_overflows"
        MC.UsbbOverflows {} -> T.pack "usbb_overflows"
        MC.SsbbOverflows {} -> T.pack "ssbb_overflows"
        MC.PopCount {} -> T.pack "popcount"
        MC.ReverseBytes {} -> T.pack "reverse_bytes"
        MC.Bsf {} -> T.pack "bsf"
        MC.Bsr {} -> T.pack "bsr"


macawPrettyOperand :: (MacawConstraints arch s) => MacawOperand arch s -> T.Text
macawPrettyOperand opr =
  case opr of
    Binder aid -> T.pack (show (MC.ppAssignId aid))
    CommentText t -> t
    AddressLiteral adr -> T.pack (show adr)
    TypeRepr tp -> T.pack (show tp)
    TypeReprs tps -> T.pack (show (FC.toListFC (\x -> show x) tps))
    Index idx -> T.pack (show idx)
    NatRepr tp -> T.pack (show tp)
    MemRepr tp -> T.pack (show tp)
    Value v -> T.pack (show v)
    Addr a -> T.pack (show a)
    SegmentOffset off -> T.pack (show off)
    JumpTable tbl -> T.pack (show tbl)
    BlockId w -> T.pack (show w)
    StateUpdate m -> prettyStateUpdate m
    RegState (MC.RegState m) -> prettyStateUpdate m
    Relocation relo -> T.pack (show relo)

prettyStateUpdate :: (ShowF (MC.ArchReg arch), MC.RegisterInfo (MC.ArchReg arch))
                  => MapF.MapF (MC.ArchReg arch) (MC.Value arch ids)
                  -> T.Text
prettyStateUpdate m =
  T.intercalate (T.pack ", ")
  [ T.pack (printf "%s -> %s" (showF reg) (show (MC.ppValue 0 val)))
  | MapF.Pair reg val <- MapF.toList m
  ]

data MacawOpcode arch s where
  Comment :: MacawOpcode arch s
  WriteMem :: MacawOpcode arch s
  Undefined :: MacawOpcode arch s
  ReadMem :: MacawOpcode arch s
  CondReadMem :: MacawOpcode arch s
  App :: MC.App (MC.Value arch ids) tp -> MacawOpcode arch s
  EvalArchFn :: MC.ArchFn arch (MC.Value arch ids) tp -> MacawOpcode arch s
  ExecArchStmt :: MC.ArchStmt arch (MC.Value arch ids) -> MacawOpcode arch s
  ArchState :: MacawOpcode arch s
  Call :: MacawOpcode arch s
  TailCall :: MacawOpcode arch s
  Jump :: MacawOpcode arch s
  IndirectJumpTable :: MacawOpcode arch s
  Return :: MacawOpcode arch s
  ITE :: MacawOpcode arch s
  TranslateError :: T.Text -> MacawOpcode arch s
  ClassifyFailure :: MacawOpcode arch s
  ArchTermStmt :: MC.ArchTermStmt arch ids -> MacawOpcode arch s

data MacawOperand arch s where
  Binder :: !(MC.AssignId ids tp) -> MacawOperand arch s
  CommentText :: !T.Text -> MacawOperand arch s
  AddressLiteral :: !(MC.ArchAddrWord arch) -> MacawOperand arch s
  TypeRepr :: !(MT.TypeRepr tp) -> MacawOperand arch s
  TypeReprs :: !(PL.List MT.TypeRepr l) -> MacawOperand arch s
  Index :: !(PL.Index l r) -> MacawOperand arch s
  NatRepr :: !(MT.NatRepr tp) -> MacawOperand arch s
  MemRepr :: !(MC.MemRepr tp) -> MacawOperand arch s
  Value :: !(MC.Value arch ids tp) -> MacawOperand arch s
  Addr :: !(MC.ArchMemAddr arch) -> MacawOperand arch s
  SegmentOffset :: !(MC.ArchSegmentOff arch) -> MacawOperand arch s
  BlockId :: !Word64 -> MacawOperand arch s
  StateUpdate :: !(MapF.MapF (MC.ArchReg arch) (MC.Value arch ids)) -> MacawOperand arch s
  RegState :: !(MC.RegState (MC.ArchReg arch) (MC.Value arch ids)) -> MacawOperand arch s
  JumpTable :: !(V.Vector (MC.ArchSegmentOff arch)) -> MacawOperand arch s
  Relocation :: !(MC.Relocation (MC.ArchAddrWidth arch)) -> MacawOperand arch s

instance NFData (MacawOperand arch s) where
  rnf !_o = ()
