{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Surveyor.Core.Architecture.Crucible (
  Crucible,
  CrucibleExtension(..),
  crucibleForMCBlocks,
  -- * Utilities for writing arch-specific backends
  toRegisterOperand,
  allocateRegister,
  toExtensionOperand,
  Instruction(..),
  Operand(..),
  CrucibleOperand(..),
  crucibleStmtOperands,
  crucibleTermStmtOperands,
  AddrK(..),
  Addr(..),
  Address(..)
  ) where

import           Control.DeepSeq ( NFData(rnf) )
import           Control.Lens ( (^.) )
import qualified Data.BitVector.Sized as DBS
import qualified Data.Foldable as F
import           Data.Functor.Const ( Const(Const, getConst) )
import qualified Data.Macaw.CFG as MC
import qualified Data.Macaw.Symbolic as MS
import qualified Data.Map as Map
import           Data.Maybe ( isJust, mapMaybe )
import           Data.Parameterized.Classes ( TestEquality(testEquality), OrdF(compareF), toOrdering )
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.NatRepr as NR
import qualified Data.Parameterized.Nonce as PN
import qualified Data.Parameterized.SymbolRepr as PS
import qualified Data.Parameterized.TH.GADT as PTH
import qualified Data.Parameterized.TraversableFC as FC
import           Data.Proxy ( Proxy(..) )
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Word ( Word64 )
import qualified Lang.Crucible.CFG.Core as C
import qualified Lang.Crucible.CFG.Expr as C
import qualified Lang.Crucible.FunctionHandle as CFH
import           Numeric ( showHex )
import           Numeric.Natural ( Natural )
import qualified Prettyprinter as PP
import qualified Renovate as R
import           Text.Printf ( printf )
import qualified What4.FunctionName as WF
import qualified What4.InterpretedFloatingPoint as WIF
import qualified What4.Symbol as WS
import qualified What4.Utils.StringLiteral as WSL

import           Surveyor.Core.Architecture.Class
import qualified Surveyor.Core.Architecture.NonceCache as SCAN
import           Surveyor.Core.IRRepr ( Crucible )
import qualified Surveyor.Core.OperandList as OL

-- | Build a 'BlockMapping' for a given function, mapping machine code blocks to
-- their crucible equivalents.  Note: there are some cases where some blocks
-- will not have a direct mapping.
--
-- We have to pass in a function to index blocks, as the metadata to enable it
-- is specific to each Crucible extension.  If a backend does not support the
-- mapping (e.g., because it doesn't have enough metadata), passing @const Map.empty@ will work.
crucibleForMCBlocks :: forall arch s
                     . ( CrucibleConstraints arch s
                       , CrucibleExtension arch
                       , CrucibleExt arch ~ MS.MacawExt arch
                       , MC.MemWidth (MC.ArchAddrWidth arch)
                       )
                    => PN.NonceGenerator IO s
                    -> ([(Block arch s, Block (Crucible arch) s)] -> Map.Map (Address (Crucible arch) s) (Set.Set (Address arch s)))
                    -> R.BlockInfo arch
                    -> R.ConcreteAddress arch
                    -> [(MC.ArchSegmentOff arch, Block arch s)]
                    -> IO (Maybe ([Block (Crucible arch) s], BlockMapping arch (Crucible arch) s))
crucibleForMCBlocks ng blockIndexer binfo faddr blocks = do
  case Map.lookup faddr (R.biCFG binfo) of
    Nothing -> return Nothing
    Just symCFGRef -> do
      C.SomeCFG symCFG <- R.getSymbolicCFG symCFGRef
      let fa = FunctionAddr (fromIntegral (R.absoluteAddress faddr))
      let fh :: FunctionHandle (Crucible arch) s
          fh = FunctionHandle { fhAddress = CrucibleAddress fa
                              , fhName = WF.functionName (CFH.handleName (C.cfgHandle symCFG))
                              }
      let blkIdx = Map.fromList [(MC.segoffAddr a, b) | (a, b) <- blocks]
      blks <- FC.traverseFC (toMCCrucibleBlock ng blkIdx fa fh) (C.cfgBlockMap symCFG)
      let irtbMap = blockIndexer (mapMaybe asJustPair (FC.toListFC getConst blks))
      let crucBlocks = FC.toListFC asCrucibleBlock blks
      let bm = BlockMapping { baseToIRAddrs = flipAddressMap irtbMap
                            , irToBaseAddrs = irtbMap
                            , blockMapping = toBlockMap (FC.toListFC getConst blks)
                            }
      return $ Just (crucBlocks, bm)
  where
    asJustPair (Just a, b) = Just (a, b)
    asJustPair (Nothing, _) = Nothing
    asCrucibleBlock (Const (_, cb)) = cb
    toBlockMap pairs = Map.fromList [ (blockAddress origBlock, (origBlock, cblock))
                                    | (Just origBlock, cblock) <- pairs
                                    ]

flipAddressMap :: (Ord (Address arch s))
               => Map.Map (Address (Crucible arch) s) (Set.Set (Address arch s))
               -> Map.Map (Address arch s) (Set.Set (Address (Crucible arch) s))
flipAddressMap = foldr doFlip Map.empty . Map.toList
  where
    doFlip (macawAddr, mcAddrs) m = F.foldr (addMachineAddrs macawAddr) m mcAddrs
    addMachineAddrs macawAddr machineAddr = Map.insertWith Set.union machineAddr (Set.singleton macawAddr)


-- | Turn a single Crucible block into a block in our representation, pairing it
-- up with the machine code block that corresponds to it.
--
-- There is no good way to establish a mapping from machine blocks to crucible
-- blocks right now, so we have to resort to a heuristic.
--
-- We can do something reasonable by looking at the archstateupdate instructions
-- to figure out which addresses are covered, and just pick the first one for
-- each crucible block.
toMCCrucibleBlock :: forall arch s blocks ret ctx
                   . ( CrucibleConstraints arch s
                     , CrucibleExtension arch
                     , CrucibleExt arch ~ MS.MacawExt arch
                     )
                  => PN.NonceGenerator IO s
                  -> Map.Map (MC.MemAddr (MC.ArchAddrWidth arch)) (Block arch s)
                  -> Addr 'FunctionK
                  -> FunctionHandle (Crucible arch) s
                  -> C.Block (CrucibleExt arch) blocks ret ctx
                  -> IO (Const (Maybe (Block arch s), Block (Crucible arch) s) ctx)
toMCCrucibleBlock ng blockIndex faddr fh b = do
  c0 <- SCAN.initialCache ng (C.blockInputs b)
  let baddr = BlockAddr faddr (Ctx.indexVal (C.blockIDIndex (C.blockID b)))
  (mMinAddr, stmts) <- buildBlock c0 baddr 0 (b ^. C.blockStmts)
  let cb = Block { blockFunction = fh
                 , blockAddress = CrucibleAddress baddr
                 , blockInstructions = stmts
                 }
  case mMinAddr of
    Just minAddr ->
      case Map.lookup minAddr blockIndex of
        Just archBlock -> return (Const (Just archBlock, cb))
        Nothing -> return (Const (Nothing, cb))
    Nothing -> return (Const (Nothing, cb))
  where
    buildBlock :: SCAN.NonceCache s ctx'
               -> Addr 'BlockK
               -> Int
               -> C.StmtSeq (CrucibleExt arch) blocks ret ctx'
               -> IO (Maybe (MC.MemAddr (MC.ArchAddrWidth arch))
                     , [(Address (Crucible arch) s, Instruction (Crucible arch) s)])
    buildBlock nc baddr iidx ss =
      case ss of
        C.ConsStmt _loc stmt ss' -> do
          (nc', mBinder, ops) <- crucibleStmtOperands nc ng stmt
          (mMinAddr, rest) <- buildBlock nc' baddr (iidx + 1) ss'
          let sz = SCAN.cacheSize nc
          let cstmt = CrucibleStmt sz stmt mBinder ops
          let iaddr = CrucibleAddress (InstructionAddr baddr iidx)
          case stmt of
            C.ExtendAssign (MS.MacawArchStateUpdate mcAddr _) ->
              -- In this case, we use the metadata instruction to attempt to
              -- figure out which machine code block this crucible block
              -- corresponds to.
              return (Just (takeMinAddr (Proxy @arch) mMinAddr mcAddr), ((iaddr, cstmt) : rest))
            _ -> return (mMinAddr, ((iaddr, cstmt) : rest))
        C.TermStmt _loc term -> do
          ops <- crucibleTermStmtOperands nc ng term
          let iaddr = CrucibleAddress (InstructionAddr baddr iidx)
          let cstmt = CrucibleTermStmt term ops
          return (Nothing, [(iaddr, cstmt)])

takeMinAddr :: proxy arch
            -> Maybe (MC.MemAddr (MC.ArchAddrWidth arch))
            -> MC.MemAddr (MC.ArchAddrWidth arch)
            -> MC.MemAddr (MC.ArchAddrWidth arch)
takeMinAddr _ Nothing a = a
takeMinAddr _ (Just a1) a2
  | a1 < a2 = a1
  | otherwise = a2

type CrucibleConstraints arch s = ( Ord (Address (Crucible arch) s)
                                  , Show (Address (Crucible arch) s)
                                  , Ord (Address arch s)
                                  , C.PrettyExt (CrucibleExt arch)
                                  )

data AddrK = FunctionK
           | BlockK
           | InstructionK

data Addr tp where
  FunctionAddr :: Word64 -> Addr 'FunctionK
  BlockAddr :: Addr 'FunctionK -> Int -> Addr 'BlockK
  InstructionAddr :: Addr 'BlockK -> Int -> Addr 'InstructionK

instance (CrucibleConstraints arch s, CrucibleExtension arch) => IR (Crucible arch) s where
  data Instruction (Crucible arch) s =
      forall ctx ctx' . CrucibleStmt (Ctx.Size ctx) (C.Stmt (CrucibleExt arch) ctx ctx') (Maybe (Operand (Crucible arch) s)) (OL.OperandList (Operand (Crucible arch) s))
    | forall blocks ret ctx . CrucibleTermStmt (C.TermStmt blocks ret ctx) (OL.OperandList (Operand (Crucible arch) s))
  data Address (Crucible arch) s = forall tp . CrucibleAddress (Addr tp)
  data Operand (Crucible arch) s = forall tp . CrucibleOperand (PN.Nonce s tp) (CrucibleOperand arch s)
  data Opcode (Crucible arch) s = CrucibleOpcode (CrucibleOpcode arch s)

  boundValue (CrucibleStmt _ _ mbv _) = mbv
  boundValue (CrucibleTermStmt {}) = Nothing

  operands (CrucibleStmt _ _ _ ops) = ops
  operands (CrucibleTermStmt _ ops) = ops

  opcode (CrucibleStmt _ s _ _) = crucibleStmtOpcode s
  opcode (CrucibleTermStmt s _) = crucibleTermStmtOpcode s

  prettyInstruction _ (CrucibleStmt sz s _ _) =
    C.ppStmt sz s
  prettyInstruction _ (CrucibleTermStmt ts _) =
    PP.pretty ts
  prettyOpcode (CrucibleOpcode o) = cruciblePrettyOpcode o
  prettyOperand _addr (CrucibleOperand _ o) = cruciblePrettyOperand o
  prettyAddress (CrucibleAddress a) =
    case a of
      FunctionAddr w -> PP.pretty "0x" <> PP.pretty (showHex w "")
      BlockAddr fa idx -> PP.viaShow fa <> PP.brackets (PP.viaShow idx)
      InstructionAddr ba idx -> PP.viaShow ba <> PP.pretty "@" <> PP.pretty idx


  parseAddress _ = Nothing
  rawRepr = Nothing
  showInstructionAddresses _ = False

  operandSelectable (CrucibleOperand _ o) =
    case o of
      BoolLit {} -> False
      NatLit {} -> False
      IntegerLit {} -> False
      BVLit {} -> False
      RationalLit {} -> False
      StringLiteral {} -> False
      FloatLit {} -> False
      DoubleLit {} -> False
      X86_80Lit {} -> False

      Reg {} -> True
      GlobalVar {} -> True
      RoundingMode {} -> False
      FnHandle {} -> True
      Index {} -> False
      BaseTerm {} -> True
      JumpTarget {} -> True

      BaseTypeRepr {} -> False
      TypeRepr {} -> False
      FloatInfoRepr {} -> False
      NatRepr {} -> False
      SymbolRepr {} -> False
      CtxRepr {} -> False
      StringInfoRepr {} -> False

      ExtensionOperand ext -> extensionOperandSelectable (Proxy @arch) ext

      Syntax {} -> False


data CrucibleOperand arch s where
  BoolLit :: Bool -> CrucibleOperand arch s
  NatLit :: Natural -> CrucibleOperand arch s
  IntegerLit :: Integer -> CrucibleOperand arch s
  BVLit :: NR.NatRepr w -> DBS.BV w -> CrucibleOperand arch s
  RationalLit :: Rational -> CrucibleOperand arch s
  StringLiteral :: WSL.StringLiteral si -> CrucibleOperand arch s
  FloatLit :: Float -> CrucibleOperand arch s
  DoubleLit :: Double -> CrucibleOperand arch s
  X86_80Lit :: WIF.X86_80Val -> CrucibleOperand arch s

  Reg :: C.Reg ctx tp -> CrucibleOperand arch s
  GlobalVar :: C.GlobalVar tp -> CrucibleOperand arch s
  RoundingMode :: C.RoundingMode -> CrucibleOperand arch s
  FnHandle :: CFH.FnHandle args ret -> CrucibleOperand arch s
  Index :: Ctx.Index ctx tp -> CrucibleOperand arch s
  BaseTerm :: C.BaseTerm (C.Reg ctx) tp -> CrucibleOperand arch s
  JumpTarget :: C.BlockID blocks args -> CrucibleOperand arch s

  BaseTypeRepr :: C.BaseTypeRepr btp -> CrucibleOperand arch s
  TypeRepr :: C.TypeRepr tp -> CrucibleOperand arch s
  FloatInfoRepr :: C.FloatInfoRepr fi -> CrucibleOperand arch s
  StringInfoRepr :: C.StringInfoRepr si -> CrucibleOperand arch s
  NatRepr :: NR.NatRepr n -> CrucibleOperand arch s
  SymbolRepr :: PS.SymbolRepr nm -> CrucibleOperand arch s
  CtxRepr :: C.CtxRepr args -> CrucibleOperand arch s

  ExtensionOperand :: CrucibleExtensionOperand arch s -> CrucibleOperand arch s

  -- Pseudo-operands for display purposes
  Syntax :: T.Text -> CrucibleOperand arch s

toExtensionOperand :: PN.Nonce s tp -> CrucibleExtensionOperand arch s -> Operand (Crucible arch) s
toExtensionOperand n eo = CrucibleOperand n (ExtensionOperand eo)

data CrucibleOpcode arch s where
  -- Normal statements
  SetReg :: C.Expr (CrucibleExt arch) ctx tp -> CrucibleOpcode arch s
  CallHandle :: CrucibleOpcode arch s
  Print :: CrucibleOpcode arch s
  ReadGlobal :: CrucibleOpcode arch s
  WriteGlobal :: CrucibleOpcode arch s
  FreshConstant :: CrucibleOpcode arch s
  FreshFloat :: CrucibleOpcode arch s
  NewRefCell :: CrucibleOpcode arch s
  NewEmptyRefCell :: CrucibleOpcode arch s
  ReadRefCell :: CrucibleOpcode arch s
  WriteRefCell :: CrucibleOpcode arch s
  DropRefCell :: CrucibleOpcode arch s
  Assert :: CrucibleOpcode arch s
  Assume :: CrucibleOpcode arch s
  ExtendAssign :: C.StmtExtension (CrucibleExt arch) (C.Reg ctx) tp -> CrucibleOpcode arch s
  -- Terminators
  Jump :: CrucibleOpcode arch s
  Br :: CrucibleOpcode arch s
  MaybeBranch :: CrucibleOpcode arch s
  VariantElim :: CrucibleOpcode arch s
  Return :: CrucibleOpcode arch s
  TailCall :: CrucibleOpcode arch s
  ErrorStmt :: CrucibleOpcode arch s

crucibleStmtOpcode :: C.Stmt (CrucibleExt arch) ctx ctx' -> Opcode (Crucible arch) s
crucibleStmtOpcode s =
  case s of
    C.SetReg _ e -> CrucibleOpcode (SetReg e)
    C.CallHandle {} -> CrucibleOpcode CallHandle
    C.Print {} -> CrucibleOpcode Print
    C.ReadGlobal {} -> CrucibleOpcode ReadGlobal
    C.WriteGlobal {} -> CrucibleOpcode WriteGlobal
    C.FreshConstant {} -> CrucibleOpcode FreshConstant
    C.FreshFloat {} -> CrucibleOpcode FreshFloat
    C.NewRefCell {} -> CrucibleOpcode NewRefCell
    C.NewEmptyRefCell {} -> CrucibleOpcode NewEmptyRefCell
    C.ReadRefCell {} -> CrucibleOpcode ReadRefCell
    C.WriteRefCell {} -> CrucibleOpcode WriteRefCell
    C.DropRefCell {} -> CrucibleOpcode DropRefCell
    C.Assert {} -> CrucibleOpcode Assert
    C.Assume {} -> CrucibleOpcode Assume
    C.ExtendAssign stmtExt -> CrucibleOpcode (ExtendAssign stmtExt)

crucibleTermStmtOpcode :: C.TermStmt blocks ret ctx -> Opcode (Crucible arch) s
crucibleTermStmtOpcode ts =
  case ts of
    C.Jump {} -> CrucibleOpcode Jump
    C.Br {} -> CrucibleOpcode Br
    C.MaybeBranch {} -> CrucibleOpcode MaybeBranch
    C.VariantElim {} -> CrucibleOpcode VariantElim
    C.Return {} -> CrucibleOpcode Return
    C.TailCall {} -> CrucibleOpcode TailCall
    C.ErrorStmt {} -> CrucibleOpcode ErrorStmt

cruciblePrettyOperand :: forall arch s ann . (CrucibleExtension arch) => CrucibleOperand arch s -> PP.Doc ann
cruciblePrettyOperand o =
  case o of
    BoolLit b -> PP.viaShow b
    NatLit n -> PP.viaShow n
    IntegerLit i -> PP.viaShow i
    BVLit nr bv -> PP.pretty (DBS.ppHex nr bv)
    RationalLit r -> PP.viaShow r
    StringLiteral s -> PP.viaShow s
    FloatLit f -> PP.viaShow f
    DoubleLit d -> PP.viaShow d
    X86_80Lit l -> PP.viaShow l

    Reg r -> PP.viaShow r
    GlobalVar g -> PP.viaShow g
    RoundingMode rm -> PP.viaShow rm
    FnHandle fh -> PP.viaShow fh
    Index idx -> PP.viaShow idx
    BaseTerm bt -> PP.viaShow (C.baseTermVal bt) <> PP.pretty ":" <> PP.viaShow (C.baseTermType bt)
    JumpTarget bid -> PP.viaShow bid

    BaseTypeRepr rep -> PP.brackets (PP.viaShow rep)
    TypeRepr rep -> PP.brackets (PP.viaShow rep)
    FloatInfoRepr rep -> PP.brackets (PP.viaShow rep)
    StringInfoRepr rep -> PP.brackets (PP.viaShow rep)
    NatRepr rep -> PP.brackets (PP.pretty "NatRepr@" <> PP.viaShow rep)
    SymbolRepr rep -> PP.brackets (PP.pretty "SymbolRepr@" <> PP.dquotes (PP.viaShow rep))
    CtxRepr rep -> PP.brackets (PP.viaShow rep)

    ExtensionOperand ce -> prettyExtensionOperand (Proxy @arch) ce

    Syntax t -> PP.pretty t

cruciblePrettyOpcode :: forall arch s ann
                      . (CrucibleConstraints arch s, CrucibleExtension arch)
                     => CrucibleOpcode arch s
                     -> PP.Doc ann
cruciblePrettyOpcode o =
  case o of
    CallHandle -> PP.pretty "call"
    Print -> PP.pretty "print"
    ReadGlobal -> PP.pretty "read-global"
    WriteGlobal -> PP.pretty "write-global"
    FreshConstant -> PP.pretty "fresh-constant"
    FreshFloat -> PP.pretty "fresh-float"
    NewRefCell -> PP.pretty "new-ref"
    NewEmptyRefCell -> PP.pretty "new-empty-ref"
    ReadRefCell -> PP.pretty "read-ref"
    WriteRefCell -> PP.pretty "write-ref"
    DropRefCell -> PP.pretty "drop-ref"
    Assert -> PP.pretty "assert"
    Assume -> PP.pretty "assume"
    ExtendAssign ext -> prettyExtensionStmt (Proxy @arch) ext
    Jump -> PP.pretty "jump"
    Br -> PP.pretty "br"
    MaybeBranch -> PP.pretty "maybe-branch"
    VariantElim -> PP.pretty "variant-elim"
    Return -> PP.pretty "return"
    TailCall -> PP.pretty "tail-call"
    ErrorStmt -> PP.pretty "error"
    SetReg (C.App app) ->
      case app of
        C.ExtensionApp extApp -> prettyExtensionApp (Proxy @arch) extApp
        C.BaseIsEq {} -> PP.pretty "eq"
        C.BaseIte {} -> PP.pretty "ite"
        C.EmptyApp -> PP.pretty "()"
        C.PackAny {} -> PP.pretty "pack"
        C.UnpackAny {} -> PP.pretty "unpack"

        C.BoolLit {} -> PP.pretty "bool-lit"
        C.Not {} -> PP.pretty "not"
        C.And {} -> PP.pretty "and"
        C.Or {} -> PP.pretty "or"
        C.BoolXor {} -> PP.pretty "xor"

        C.NatLit {} -> PP.pretty "nat-lit"
        C.NatLt {} -> PP.pretty "nat-lt"
        C.NatLe {} -> PP.pretty "nat-le"
        C.NatAdd {} -> PP.pretty "nat-add"
        C.NatSub {} -> PP.pretty "nat-sub"
        C.NatMul {} -> PP.pretty "nat-mul"
        C.NatDiv {} -> PP.pretty "nat-div"
        C.NatMod {} -> PP.pretty "nat-mod"

        C.IntLit {} -> PP.pretty "int-lit"
        C.IntLt {} -> PP.pretty "int-lt"
        C.IntLe {} -> PP.pretty "int-le"
        C.IntNeg {} -> PP.pretty "int-neg"
        C.IntAdd {} -> PP.pretty "int-add"
        C.IntSub {} -> PP.pretty "int-sub"
        C.IntMul {} -> PP.pretty "int-mul"
        C.IntDiv {} -> PP.pretty "int-div"
        C.IntMod {} -> PP.pretty "int-mod"
        C.IntAbs {} -> PP.pretty "int-abs"

        C.RationalLit {} -> PP.pretty "rational-lit"
        C.RealLt {} -> PP.pretty "real-lt"
        C.RealLe {} -> PP.pretty "real-le"
        C.RealNeg {} -> PP.pretty "real-neg"
        C.RealAdd {} -> PP.pretty "real-add"
        C.RealSub {} -> PP.pretty "real-sub"
        C.RealMul {} -> PP.pretty "real-mul"
        C.RealDiv {} -> PP.pretty "real-div"
        C.RealMod {} -> PP.pretty "real-mod"
        C.RealIsInteger {} -> PP.pretty "real-is-integer"

        C.FloatLit {} -> PP.pretty "float-lit"
        C.DoubleLit {} -> PP.pretty "double-lit"
        C.X86_80Lit {} -> PP.pretty "x86_80-lit"
        C.FloatNaN {} -> PP.pretty "float-nan"
        C.FloatPInf {} -> PP.pretty "float-pinf"
        C.FloatNInf {} -> PP.pretty "float-ninf"
        C.FloatPZero {} -> PP.pretty "float-pzero"
        C.FloatNZero {} -> PP.pretty "float-nzero"
        C.FloatNeg {} -> PP.pretty "float-neg"
        C.FloatAbs {} -> PP.pretty "float-abs"
        C.FloatSqrt {} -> PP.pretty "float-sqrt"
        C.FloatAdd {} -> PP.pretty "float-add"
        C.FloatSub {} -> PP.pretty "float-sub"
        C.FloatMul {} -> PP.pretty "float-mul"
        C.FloatDiv {} -> PP.pretty "float-div"
        C.FloatRem {} -> PP.pretty "float-rem"
        C.FloatMin {} -> PP.pretty "float-min"
        C.FloatMax {} -> PP.pretty "float-max"
        C.FloatFMA {} -> PP.pretty "float-fma"
        C.FloatEq {} -> PP.pretty "float-eq"
        C.FloatFpEq {} -> PP.pretty "float-fpeq"
        C.FloatGt {} -> PP.pretty "float-gt"
        C.FloatGe {} -> PP.pretty "float-ge"
        C.FloatLt {} -> PP.pretty "float-lt"
        C.FloatLe {} -> PP.pretty "float-le"
        C.FloatNe {} -> PP.pretty "float-ne"
        C.FloatFpNe {} -> PP.pretty "float-fpne"
        C.FloatIte {} -> PP.pretty "float-ite"
        C.FloatCast {} -> PP.pretty "float-cast"
        C.FloatFromBinary {} -> PP.pretty "float-from-binary"
        C.FloatToBinary {} -> PP.pretty "float-to-binary"
        C.FloatFromBV {} -> PP.pretty "float-from-bv"
        C.FloatFromSBV {} -> PP.pretty "float-from-sbv"
        C.FloatFromReal {} -> PP.pretty "float-from-real"
        C.FloatToBV {} -> PP.pretty "float-to-bv"
        C.FloatToSBV {} -> PP.pretty "float-to-sbv"
        C.FloatToReal {} -> PP.pretty "float-to-real"
        C.FloatIsNaN {} -> PP.pretty "float-is-nan"
        C.FloatIsInfinite {} -> PP.pretty "float-is-infinite"
        C.FloatIsZero {} -> PP.pretty "float-is-zero"
        C.FloatIsPositive {} -> PP.pretty "float-is-positive"
        C.FloatIsNegative {} -> PP.pretty "float-is-negative"
        C.FloatIsSubnormal {} -> PP.pretty "float-is-subnormal"
        C.FloatIsNormal {} -> PP.pretty "float-is-normal"

        C.JustValue {} -> PP.pretty "just"
        C.NothingValue {} -> PP.pretty "nothing"
        C.FromJustValue {} -> PP.pretty "from-just"

        C.RollRecursive {} -> PP.pretty "roll"
        C.UnrollRecursive {} -> PP.pretty "unroll"

        C.VectorLit {} -> PP.pretty "vector-lit"
        C.VectorReplicate {} -> PP.pretty "vector-replicate"
        C.VectorIsEmpty {} -> PP.pretty "vector-is-empty"
        C.VectorSize {} -> PP.pretty "vector-size"
        C.VectorGetEntry {} -> PP.pretty "vector-get"
        C.VectorSetEntry {} -> PP.pretty "vector-set"
        C.VectorCons {} -> PP.pretty "vector-cons"

        C.HandleLit {} -> PP.pretty "handle-lit"
        C.Closure {} -> PP.pretty "closure"

        C.NatToInteger {} -> PP.pretty "nat-to-integer"
        C.IntegerToReal {} -> PP.pretty "integer-to-real"
        C.RealRound {} -> PP.pretty "real-round"
        C.RealFloor {} -> PP.pretty "real-floor"
        C.RealCeil {} -> PP.pretty "real-ceil"
        C.IntegerToBV {} -> PP.pretty "integer-to-bv"
        C.RealToNat {} -> PP.pretty "real-to-nat"

        C.Complex {} -> PP.pretty "complex"
        C.RealPart {} -> PP.pretty "real-part"
        C.ImagPart {} -> PP.pretty "imag-part"

        C.BVUndef {} -> PP.pretty "bv-undef"
        C.BVLit {} -> PP.pretty "bv-lit"
        C.BVConcat {} -> PP.pretty "bv-concat"
        C.BVSelect {} -> PP.pretty "bv-select"
        C.BVTrunc {} -> PP.pretty "bv-trunc"
        C.BVZext {} -> PP.pretty "bv-zext"
        C.BVSext {} -> PP.pretty "bv-sext"
        C.BVNot {} -> PP.pretty "bv-not"
        C.BVAnd {} -> PP.pretty "bv-and"
        C.BVOr {} -> PP.pretty "bv-or"
        C.BVXor {} -> PP.pretty "bv-xor"
        C.BVNeg {} -> PP.pretty "bv-neg"
        C.BVAdd {} -> PP.pretty "bv-add"
        C.BVSub {} -> PP.pretty "bv-sub"
        C.BVMul {} -> PP.pretty "bv-mul"
        C.BVUdiv {} -> PP.pretty "bv-udiv"
        C.BVSdiv {} -> PP.pretty "bv-sdiv"
        C.BVUrem {} -> PP.pretty "bv-urem"
        C.BVSrem {} -> PP.pretty "bv-srem"
        C.BVUle {} -> PP.pretty "bv-ule"
        C.BVUlt {} -> PP.pretty "bv-ult"
        C.BVSle {} -> PP.pretty "bv-sle"
        C.BVSlt {} -> PP.pretty "bv-slt"
        C.BVCarry {} -> PP.pretty "bv-carry"
        C.BVSCarry {} -> PP.pretty "bv-scarry"
        C.BVSBorrow {} -> PP.pretty "bv-sborrow"
        C.BVShl {} -> PP.pretty "bv-shl"
        C.BVLshr {} -> PP.pretty "bv-lshr"
        C.BVAshr {} -> PP.pretty "bv-ashr"
        C.BoolToBV {} -> PP.pretty "bool-to-bv"
        C.BvToInteger {} -> PP.pretty "bv-to-integer"
        C.SbvToInteger {} -> PP.pretty "sbv-to-integer"
        C.BvToNat {} -> PP.pretty "bv-to-nat"
        C.BVNonzero {} -> PP.pretty "bv-nonzero"
        C.BVUMin {} -> PP.pretty "bv-umin"
        C.BVUMax {} -> PP.pretty "bv-umax"
        C.BVSMin {} -> PP.pretty "bv-smin"
        C.BVSMax {} -> PP.pretty "bv-smax"

        C.EmptyWordMap {} -> PP.pretty "empty-wordmap"
        C.InsertWordMap {} -> PP.pretty "insert-wordmap"
        C.LookupWordMap {} -> PP.pretty "lookup-wordmap"
        C.LookupWordMapWithDefault {} -> PP.pretty "lookup-wordmap-with-default"

        C.InjectVariant {} -> PP.pretty "inject"
        C.ProjectVariant {} -> PP.pretty "project"

        C.MkStruct {} -> PP.pretty "mk-struct"
        C.GetStruct {} -> PP.pretty "get-struct"
        C.SetStruct {} -> PP.pretty "set-struct"

        C.EmptyStringMap {} -> PP.pretty "empty-stringmap"
        C.LookupStringMapEntry {} -> PP.pretty "lookup-stringmap"
        C.InsertStringMapEntry {} -> PP.pretty "insert-stringmap"

        C.StringLit {} -> PP.pretty "string-lit"
        C.StringEmpty {} -> PP.pretty "string-empty"
        C.StringConcat {} -> PP.pretty "string-concat"
        C.StringLength {} -> PP.pretty "string-length"
        C.StringContains {} -> PP.pretty "string-contains"
        C.StringIsPrefixOf {} -> PP.pretty "string-is-prefix-of"
        C.StringIsSuffixOf {} -> PP.pretty "string-is-suffix-of"
        C.StringIndexOf {} -> PP.pretty "string-index-of"
        C.StringSubstring {} -> PP.pretty "string-substring"
        C.ShowValue {} -> PP.pretty "show-value"
        C.ShowFloat {} -> PP.pretty "show-float"

        C.SymArrayLookup {} -> PP.pretty "symarray-lookup"
        C.SymArrayUpdate {} -> PP.pretty "symarray-update"

        C.IsConcrete {} -> PP.pretty "is-concrete"
        C.ReferenceEq {} -> PP.pretty "reference-eq"

toRegisterOperand :: SCAN.NonceCache s ctx
                  -> C.Reg ctx tp
                  -> Operand (Crucible arch) s
toRegisterOperand (SCAN.NonceCache cache) r =
  CrucibleOperand (cache Ctx.! C.regIndex r) (Reg r)

allocateRegister :: SCAN.NonceCache s ctx
                 -> PN.NonceGenerator IO s
                 -> Maybe (C.Reg (ctx Ctx.::> tp) tp -> CrucibleOperand arch s)
                 -> IO (SCAN.NonceCache s (ctx Ctx.::> tp), Operand (Crucible arch) s)
allocateRegister (SCAN.NonceCache cache) ng mTyCon = do
  n <- PN.freshNonce ng
  let creg = C.Reg (Ctx.nextIndex (Ctx.size cache))
  case mTyCon of
    Nothing -> do
      let reg = CrucibleOperand n (Reg creg)
      return (SCAN.NonceCache (Ctx.extend cache n), reg)
    Just con -> return (SCAN.NonceCache (Ctx.extend cache n), CrucibleOperand n (con creg))

-- | Extract operands from a terminal statement
--
-- Terminal statements cannot bind new values, so this function does not need to
-- update the cache or return binders.
crucibleTermStmtOperands :: SCAN.NonceCache s ctx
                         -> PN.NonceGenerator IO s
                         -> C.TermStmt blocks ret ctx
                         -> IO (OL.OperandList (Operand (Crucible arch) s))
crucibleTermStmtOperands cache ng stmt =
  case stmt of
    C.Jump (C.JumpTarget bid _ args) -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromItemList [ OL.Item (CrucibleOperand n1 (JumpTarget bid))
                            , OL.Delimited OL.Parens (OL.fromList (FC.toListFC (toRegisterOperand cache) args))
                            ]
    C.Br r (C.JumpTarget bid1 _ args1) (C.JumpTarget bid2 _ args2) -> do
      n1 <- PN.freshNonce ng
      n4 <- PN.freshNonce ng
      return $ OL.fromItemList [ OL.Item (toRegisterOperand cache r)
                            , OL.Item (CrucibleOperand n1 (JumpTarget bid1))
                            , OL.Delimited OL.Parens (OL.fromList (FC.toListFC (toRegisterOperand cache) args1))
                            , OL.Item (CrucibleOperand n4 (JumpTarget bid2))
                            , OL.Delimited OL.Parens (OL.fromList (FC.toListFC (toRegisterOperand cache) args2))
                            ]
    C.MaybeBranch trep r (C.SwitchTarget bid1 _ args1) (C.JumpTarget bid2 _ args2) -> do
      n1 <- PN.freshNonce ng
      n2 <- PN.freshNonce ng
      n3 <- PN.freshNonce ng
      return $ OL.fromItemList [ OL.Item (CrucibleOperand n1 (TypeRepr trep))
                            , OL.Item (toRegisterOperand cache r)
                            , OL.Item (CrucibleOperand n2 (JumpTarget bid1))
                            , OL.Delimited OL.Parens (OL.fromList (FC.toListFC (toRegisterOperand cache) args1))
                            , OL.Item (CrucibleOperand n3 (JumpTarget bid2))
                            , OL.Delimited OL.Parens (OL.fromList (FC.toListFC (toRegisterOperand cache) args2))
                            ]
    C.VariantElim _ r targets -> return (OL.fromList [toRegisterOperand cache r])
    C.TailCall r _ args -> do
      return $ OL.fromItemList [ OL.Item (toRegisterOperand cache r)
                            , OL.Delimited OL.Parens (OL.fromList (FC.toListFC (toRegisterOperand cache) args))
                            ]
    C.Return r -> return (OL.fromList [toRegisterOperand cache r])
    C.ErrorStmt r -> return (OL.fromList [toRegisterOperand cache r])

crucibleStmtOperands :: (CrucibleExtension arch)
                     => SCAN.NonceCache s ctx
                     -> PN.NonceGenerator IO s
                     -> C.Stmt (CrucibleExt arch) ctx ctx'
                     -> IO (SCAN.NonceCache s ctx', Maybe (Operand (Crucible arch) s), OL.OperandList (Operand (Crucible arch) s))
crucibleStmtOperands cache ng stmt =
  case stmt of
    C.Print r ->
      return (cache, Nothing, OL.fromList [toRegisterOperand cache r])
    C.WriteGlobal gv r -> do
      n1 <- PN.freshNonce ng
      return (cache, Nothing, OL.fromList [ CrucibleOperand n1 (GlobalVar gv)
                                       , toRegisterOperand cache r
                                       ])
    C.ReadGlobal gv -> do
      n1 <- PN.freshNonce ng
      (nc', binder) <- allocateRegister cache ng Nothing
      return (nc', Just binder, OL.fromList [CrucibleOperand n1 (GlobalVar gv)])
    C.FreshConstant tp msym -> do
      n2 <- PN.freshNonce ng
      n3 <- PN.freshNonce ng
      (nc', binder) <- allocateRegister cache ng Nothing
      case msym of
        Nothing -> return (nc', Just binder, OL.fromList [CrucibleOperand n2 (BaseTypeRepr tp)])
        Just sym -> return (nc', Just binder, OL.fromList [ CrucibleOperand n2 (BaseTypeRepr tp)
                                              , CrucibleOperand n3 (StringLiteral (WSL.UnicodeLiteral (WS.solverSymbolAsText sym)))
                                              ])
    C.FreshFloat fi msym -> do
      n1 <- PN.freshNonce ng
      n2 <- PN.freshNonce ng
      (nc', binder) <- allocateRegister cache ng Nothing
      case msym of
        Nothing -> return (nc', Just binder, OL.fromList [CrucibleOperand n1 (FloatInfoRepr fi)])
        Just sym -> return (nc', Just binder, OL.fromList [ CrucibleOperand n1 (FloatInfoRepr fi)
                                                       , CrucibleOperand n2 (StringLiteral (WSL.UnicodeLiteral (WS.solverSymbolAsText sym)))
                                                       ])
    C.NewRefCell tp r -> do
      n1 <- PN.freshNonce ng
      (nc', binder) <- allocateRegister cache ng Nothing
      return (nc', Just binder, OL.fromList [ CrucibleOperand n1 (TypeRepr tp)
                                , toRegisterOperand cache r
                                ])
    C.NewEmptyRefCell tp -> do
      n1 <- PN.freshNonce ng
      (nc', binder) <- allocateRegister cache ng Nothing
      return (nc', Just binder, OL.fromList [CrucibleOperand n1 (TypeRepr tp)])
    C.ReadRefCell r -> do
      (nc', binder) <- allocateRegister cache ng Nothing
      return (nc', Just binder, OL.fromList [toRegisterOperand cache r])
    C.WriteRefCell rr r -> do
      return (cache, Nothing, OL.fromList [toRegisterOperand cache rr, toRegisterOperand cache r])
    C.DropRefCell r ->
      return (cache, Nothing, OL.fromList [toRegisterOperand cache r])
    C.Assert r msg ->
      return (cache, Nothing, OL.fromList [toRegisterOperand cache r, toRegisterOperand cache msg])
    C.Assume r msg ->
      return (cache, Nothing, OL.fromList [toRegisterOperand cache r, toRegisterOperand cache msg])
    C.ExtendAssign ext -> do
      (nc', binder) <- allocateRegister cache ng Nothing
      ops <- extensionStmtOperands cache ng ext
      return (nc', Just binder, OL.fromList ops)
    C.CallHandle rep fh _reprs args -> do
      n1 <- PN.freshNonce ng
      (nc', binder) <- allocateRegister cache ng Nothing
      let opl = OL.fromItemList [ OL.Item (CrucibleOperand n1 (TypeRepr rep))
                             , OL.Item (toRegisterOperand cache fh)
                             , OL.Delimited OL.Parens (OL.fromList (FC.toListFC (toRegisterOperand cache) args))
                             ]
      return (nc', Just binder, opl)
    C.SetReg _tp (C.App app) -> do
      (nc', binder) <- allocateRegister cache ng Nothing
      ops <- crucibleAppOperands cache ng app
      return (nc', Just binder, ops)

-- | Note that this cannot add cache entries
crucibleAppOperands :: (CrucibleExtension arch)
                    => SCAN.NonceCache s ctx
                    -> PN.NonceGenerator IO s
                    -> C.App (CrucibleExt arch) (C.Reg ctx) tp
                    -> IO (OL.OperandList (Operand (Crucible arch) s))
crucibleAppOperands cache ng app =
  case app of
    C.BaseIsEq btp r1 r2 -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (BaseTypeRepr btp)
                        , toRegisterOperand cache r1
                        , toRegisterOperand cache r2
                        ]
    C.BaseIte btp r1 r2 r3 -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (BaseTypeRepr btp)
             , toRegisterOperand cache r1
             , toRegisterOperand cache r2
             , toRegisterOperand cache r3
             ]
    C.EmptyApp -> return $ OL.fromList []

    C.PackAny tp r -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (TypeRepr tp)
             , toRegisterOperand cache r
             ]
    C.UnpackAny tp r -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (TypeRepr tp)
             , toRegisterOperand cache r
             ]

    C.BoolLit b -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (BoolLit b) ]
    C.Not r -> return $ OL.fromList [ toRegisterOperand cache r ]
    C.And r1 r2 -> return $ OL.fromList [ toRegisterOperand cache r1, toRegisterOperand cache r2 ]
    C.Or r1 r2 -> return $ OL.fromList [ toRegisterOperand cache r1, toRegisterOperand cache r2 ]
    C.BoolXor r1 r2 -> return $ OL.fromList [ toRegisterOperand cache r1, toRegisterOperand cache r2 ]

    C.NatLit nat -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (NatLit nat) ]
    C.NatLt r1 r2 -> return $ OL.fromList [ toRegisterOperand cache r1, toRegisterOperand cache r2 ]
    C.NatLe r1 r2 -> return $ OL.fromList [ toRegisterOperand cache r1, toRegisterOperand cache r2 ]
    C.NatAdd r1 r2 -> return $ OL.fromList [ toRegisterOperand cache r1, toRegisterOperand cache r2 ]
    C.NatSub r1 r2 -> return $ OL.fromList [ toRegisterOperand cache r1, toRegisterOperand cache r2 ]
    C.NatMul r1 r2 -> return $ OL.fromList [ toRegisterOperand cache r1, toRegisterOperand cache r2 ]
    C.NatDiv r1 r2 -> return $ OL.fromList [ toRegisterOperand cache r1, toRegisterOperand cache r2 ]
    C.NatMod r1 r2 -> return $ OL.fromList [ toRegisterOperand cache r1, toRegisterOperand cache r2 ]

    C.IntLit i -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (IntegerLit i) ]
    C.IntLt r1 r2 -> return $ OL.fromList [ toRegisterOperand cache r1, toRegisterOperand cache r2 ]
    C.IntLe r1 r2 -> return $ OL.fromList [ toRegisterOperand cache r1, toRegisterOperand cache r2 ]
    C.IntNeg r1 -> return $ OL.fromList [ toRegisterOperand cache r1 ]
    C.IntAdd r1 r2 -> return $ OL.fromList [ toRegisterOperand cache r1, toRegisterOperand cache r2 ]
    C.IntSub r1 r2 -> return $ OL.fromList [ toRegisterOperand cache r1, toRegisterOperand cache r2 ]
    C.IntMul r1 r2 -> return $ OL.fromList [ toRegisterOperand cache r1, toRegisterOperand cache r2 ]
    C.IntDiv r1 r2 -> return $ OL.fromList [ toRegisterOperand cache r1, toRegisterOperand cache r2 ]
    C.IntMod r1 r2 -> return $ OL.fromList [ toRegisterOperand cache r1, toRegisterOperand cache r2 ]
    C.IntAbs r1 -> return $ OL.fromList [ toRegisterOperand cache r1 ]

    C.RationalLit r -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (RationalLit r) ]
    C.RealLt r1 r2 -> return $ OL.fromList [ toRegisterOperand cache r1, toRegisterOperand cache r2 ]
    C.RealLe r1 r2 -> return $ OL.fromList [ toRegisterOperand cache r1, toRegisterOperand cache r2 ]
    C.RealNeg r -> return $ OL.fromList [ toRegisterOperand cache r ]
    C.RealAdd r1 r2 -> return $ OL.fromList [ toRegisterOperand cache r1, toRegisterOperand cache r2 ]
    C.RealSub r1 r2 -> return $ OL.fromList [ toRegisterOperand cache r1, toRegisterOperand cache r2 ]
    C.RealMul r1 r2 -> return $ OL.fromList [ toRegisterOperand cache r1, toRegisterOperand cache r2 ]
    C.RealDiv r1 r2 -> return $ OL.fromList [ toRegisterOperand cache r1, toRegisterOperand cache r2 ]
    C.RealMod r1 r2 -> return $ OL.fromList [ toRegisterOperand cache r1, toRegisterOperand cache r2 ]
    C.RealIsInteger r -> return $ OL.fromList [ toRegisterOperand cache r ]

    C.FloatLit f -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (FloatLit f) ]
    C.DoubleLit d -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (DoubleLit d) ]
    C.X86_80Lit l -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (X86_80Lit l) ]
    C.FloatNaN frep -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (FloatInfoRepr frep) ]
    C.FloatPInf frep -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (FloatInfoRepr frep) ]
    C.FloatNInf frep -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (FloatInfoRepr frep) ]
    C.FloatPZero frep -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (FloatInfoRepr frep) ]
    C.FloatNZero frep -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (FloatInfoRepr frep) ]
    C.FloatNeg frep r -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (FloatInfoRepr frep)
             , toRegisterOperand cache r
             ]
    C.FloatAbs frep r -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (FloatInfoRepr frep)
             , toRegisterOperand cache r
             ]
    C.FloatSqrt frep rm r -> do
      n1 <- PN.freshNonce ng
      n2 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (FloatInfoRepr frep)
             , CrucibleOperand n2 (RoundingMode rm)
             , toRegisterOperand cache r
             ]
    C.FloatAdd frep rm r1 r2 -> do
      n1 <- PN.freshNonce ng
      n2 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (FloatInfoRepr frep)
             , CrucibleOperand n2 (RoundingMode rm)
             , toRegisterOperand cache r1
             , toRegisterOperand cache r2
             ]
    C.FloatSub frep rm r1 r2 -> do
      n1 <- PN.freshNonce ng
      n2 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (FloatInfoRepr frep)
             , CrucibleOperand n2 (RoundingMode rm)
             , toRegisterOperand cache r1
             , toRegisterOperand cache r2
             ]
    C.FloatMul frep rm r1 r2 -> do
      n1 <- PN.freshNonce ng
      n2 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (FloatInfoRepr frep)
             , CrucibleOperand n2 (RoundingMode rm)
             , toRegisterOperand cache r1
             , toRegisterOperand cache r2
             ]
    C.FloatDiv frep rm r1 r2 -> do
      n1 <- PN.freshNonce ng
      n2 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (FloatInfoRepr frep)
             , CrucibleOperand n2 (RoundingMode rm)
             , toRegisterOperand cache r1
             , toRegisterOperand cache r2
             ]
    C.FloatRem frep r1 r2 -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (FloatInfoRepr frep)
             , toRegisterOperand cache r1
             , toRegisterOperand cache r2
             ]
    C.FloatMin frep r1 r2 -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (FloatInfoRepr frep)
             , toRegisterOperand cache r1
             , toRegisterOperand cache r2
             ]
    C.FloatMax frep r1 r2 -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (FloatInfoRepr frep)
             , toRegisterOperand cache r1
             , toRegisterOperand cache r2
             ]
    C.FloatFMA frep rm r1 r2 r3 -> do
      n1 <- PN.freshNonce ng
      n2 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (FloatInfoRepr frep)
             , CrucibleOperand n2 (RoundingMode rm)
             , toRegisterOperand cache r1
             , toRegisterOperand cache r2
             , toRegisterOperand cache r3
             ]
    C.FloatEq r1 r2 -> return $ OL.fromList [ toRegisterOperand cache r1, toRegisterOperand cache r2 ]
    C.FloatFpEq r1 r2 -> return $ OL.fromList [ toRegisterOperand cache r1, toRegisterOperand cache r2 ]
    C.FloatGt r1 r2 -> return $ OL.fromList [ toRegisterOperand cache r1, toRegisterOperand cache r2 ]
    C.FloatGe r1 r2 -> return $ OL.fromList [ toRegisterOperand cache r1, toRegisterOperand cache r2 ]
    C.FloatLt r1 r2 -> return $ OL.fromList [ toRegisterOperand cache r1, toRegisterOperand cache r2 ]
    C.FloatLe r1 r2 -> return $ OL.fromList [ toRegisterOperand cache r1 , toRegisterOperand cache r2 ]
    C.FloatNe r1 r2 -> return $ OL.fromList [ toRegisterOperand cache r1, toRegisterOperand cache r2 ]
    C.FloatFpNe r1 r2 -> return $ OL.fromList [ toRegisterOperand cache r1, toRegisterOperand cache r2 ]
    C.FloatIte frep r1 r2 r3 -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (FloatInfoRepr frep)
             , toRegisterOperand cache r1
             , toRegisterOperand cache r2
             , toRegisterOperand cache r3
             ]
    C.FloatCast frep rm r -> do
      n1 <- PN.freshNonce ng
      n2 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (FloatInfoRepr frep)
             , CrucibleOperand n2 (RoundingMode rm)
             , toRegisterOperand cache r
             ]
    C.FloatFromBinary frep r -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (FloatInfoRepr frep)
             , toRegisterOperand cache r
             ]
    C.FloatToBinary frep r -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (FloatInfoRepr frep)
             , toRegisterOperand cache r
             ]
    C.FloatFromBV frep rm r -> do
      n1 <- PN.freshNonce ng
      n2 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (FloatInfoRepr frep)
             , CrucibleOperand n2 (RoundingMode rm)
             , toRegisterOperand cache r
             ]
    C.FloatFromSBV frep rm r -> do
      n1 <- PN.freshNonce ng
      n2 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (FloatInfoRepr frep)
             , CrucibleOperand n2 (RoundingMode rm)
             , toRegisterOperand cache r
             ]
    C.FloatFromReal frep rm r -> do
      n1 <- PN.freshNonce ng
      n2 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (FloatInfoRepr frep)
             , CrucibleOperand n2 (RoundingMode rm)
             , toRegisterOperand cache r
             ]
    C.FloatToBV nrep rm r -> do
      n1 <- PN.freshNonce ng
      n2 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (NatRepr nrep)
             , CrucibleOperand n2 (RoundingMode rm)
             , toRegisterOperand cache r
             ]
    C.FloatToSBV nrep rm r -> do
      n1 <- PN.freshNonce ng
      n2 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (NatRepr nrep)
             , CrucibleOperand n2 (RoundingMode rm)
             , toRegisterOperand cache r
             ]
    C.FloatToReal r -> return $ OL.fromList [ toRegisterOperand cache r ]
    C.FloatIsNaN r -> return $ OL.fromList [ toRegisterOperand cache r ]
    C.FloatIsInfinite r -> return $ OL.fromList [ toRegisterOperand cache r ]
    C.FloatIsZero r -> return $ OL.fromList [ toRegisterOperand cache r ]
    C.FloatIsPositive r -> return $ OL.fromList [ toRegisterOperand cache r ]
    C.FloatIsNegative r -> return $ OL.fromList [ toRegisterOperand cache r ]
    C.FloatIsSubnormal r -> return $ OL.fromList [ toRegisterOperand cache r ]
    C.FloatIsNormal r -> return $ OL.fromList [ toRegisterOperand cache r ]

    C.JustValue trep r -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (TypeRepr trep)
             , toRegisterOperand cache r
             ]
    C.NothingValue trep -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (TypeRepr trep) ]
    C.FromJustValue trep r1 r2 -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (TypeRepr trep)
             , toRegisterOperand cache r1
             , toRegisterOperand cache r2
             ]

    C.RollRecursive sr cr r -> do
      n1 <- PN.freshNonce ng
      n2 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (SymbolRepr sr)
             , CrucibleOperand n2 (CtxRepr cr)
             , toRegisterOperand cache r
             ]
    C.UnrollRecursive sr cr r -> do
      n1 <- PN.freshNonce ng
      n2 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (SymbolRepr sr)
             , CrucibleOperand n2 (CtxRepr cr)
             , toRegisterOperand cache r
             ]

    C.VectorLit trep vrs -> do
      n1 <- PN.freshNonce ng
      -- FIXME: It might be nice to syntactically differentiate the type list elts
      return $ OL.fromItemList [ OL.Item (CrucibleOperand n1 (TypeRepr trep))
                            , OL.Delimited OL.Angles (OL.fromList (map (toRegisterOperand cache) (F.toList vrs)))
                            ]
    C.VectorReplicate trep r1 r2 -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (TypeRepr trep)
             , toRegisterOperand cache r1
             , toRegisterOperand cache r2
             ]
    C.VectorIsEmpty r -> return $ OL.fromList [ toRegisterOperand cache r ]
    C.VectorSize r -> return $ OL.fromList [ toRegisterOperand cache r ]
    C.VectorGetEntry trep r1 r2 -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (TypeRepr trep)
             , toRegisterOperand cache r1
             , toRegisterOperand cache r2
             ]
    C.VectorSetEntry trep r1 r2 r3 -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (TypeRepr trep)
             , toRegisterOperand cache r1
             , toRegisterOperand cache r2
             , toRegisterOperand cache r3
             ]
    C.VectorCons trep r1 r2 -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (TypeRepr trep)
             , toRegisterOperand cache r1
             , toRegisterOperand cache r2
             ]

    C.HandleLit hdl -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (FnHandle hdl) ]

    C.Closure crep trep1 r1 trep2 r2 -> do
      n1 <- PN.freshNonce ng
      n2 <- PN.freshNonce ng
      n3 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (CtxRepr crep)
             , CrucibleOperand n2 (TypeRepr trep1)
             , toRegisterOperand cache r1
             , CrucibleOperand n3 (TypeRepr trep2)
             , toRegisterOperand cache r2
             ]

    C.NatToInteger r -> return $ OL.fromList [ toRegisterOperand cache r ]
    C.IntegerToReal r -> return $ OL.fromList [ toRegisterOperand cache r ]
    C.RealRound r -> return $ OL.fromList [ toRegisterOperand cache r ]
    C.RealFloor r -> return $ OL.fromList [ toRegisterOperand cache r ]
    C.RealCeil r -> return $ OL.fromList [ toRegisterOperand cache r ]
    C.IntegerToBV nrep r -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (NatRepr nrep)
             , toRegisterOperand cache r
             ]
    C.RealToNat r -> return $ OL.fromList [ toRegisterOperand cache r ]

    C.Complex r1 r2 -> return $ OL.fromList [ toRegisterOperand cache r1, toRegisterOperand cache r2 ]
    C.RealPart r -> return $ OL.fromList [ toRegisterOperand cache r ]
    C.ImagPart r -> return $ OL.fromList [ toRegisterOperand cache r ]

    C.BVUndef nrep -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (NatRepr nrep) ]
    C.BVLit nr bv -> do
      n1 <- PN.freshNonce ng
      n2 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (NatRepr nr)
             , CrucibleOperand n2 (BVLit nr bv)
             ]
    C.BVConcat nrep1 nrep2 r1 r2 -> do
      n1 <- PN.freshNonce ng
      n2 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (NatRepr nrep1)
             , CrucibleOperand n2 (NatRepr nrep2)
             , toRegisterOperand cache r1
             , toRegisterOperand cache r2
             ]
    C.BVSelect nrep1 nrep2 nrep3 r -> do
      n1 <- PN.freshNonce ng
      n2 <- PN.freshNonce ng
      n3 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (NatRepr nrep1)
             , CrucibleOperand n2 (NatRepr nrep2)
             , CrucibleOperand n3 (NatRepr nrep3)
             , toRegisterOperand cache r
             ]
    C.BVTrunc nrep1 nrep2 r -> do
      n1 <- PN.freshNonce ng
      n2 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (NatRepr nrep1)
             , CrucibleOperand n2 (NatRepr nrep2)
             , toRegisterOperand cache r
             ]
    C.BVZext nrep1 nrep2 r -> do
      n1 <- PN.freshNonce ng
      n2 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (NatRepr nrep1)
             , CrucibleOperand n2 (NatRepr nrep2)
             , toRegisterOperand cache r
             ]
    C.BVSext nrep1 nrep2 r -> do
      n1 <- PN.freshNonce ng
      n2 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (NatRepr nrep1)
             , CrucibleOperand n2 (NatRepr nrep2)
             , toRegisterOperand cache r
             ]
    C.BVNot nrep r -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (NatRepr nrep)
             , toRegisterOperand cache r
             ]
    C.BVAnd nrep r1 r2 -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (NatRepr nrep)
             , toRegisterOperand cache r1
             , toRegisterOperand cache r2
             ]
    C.BVOr nrep r1 r2 -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (NatRepr nrep)
             , toRegisterOperand cache r1
             , toRegisterOperand cache r2
             ]
    C.BVXor nrep r1 r2 -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (NatRepr nrep)
             , toRegisterOperand cache r1
             , toRegisterOperand cache r2
             ]
    C.BVNeg nrep r -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (NatRepr nrep)
             , toRegisterOperand cache r
             ]
    C.BVAdd nrep r1 r2 -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (NatRepr nrep)
             , toRegisterOperand cache r1
             , toRegisterOperand cache r2
             ]
    C.BVSub nrep r1 r2 -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (NatRepr nrep)
             , toRegisterOperand cache r1
             , toRegisterOperand cache r2
             ]
    C.BVMul nrep r1 r2 -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (NatRepr nrep)
             , toRegisterOperand cache r1
             , toRegisterOperand cache r2
             ]
    C.BVUdiv nrep r1 r2 -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (NatRepr nrep)
             , toRegisterOperand cache r1
             , toRegisterOperand cache r2
             ]
    C.BVSdiv nrep r1 r2 -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (NatRepr nrep)
             , toRegisterOperand cache r1
             , toRegisterOperand cache r2
             ]
    C.BVUrem nrep r1 r2 -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (NatRepr nrep)
             , toRegisterOperand cache r1
             , toRegisterOperand cache r2
             ]
    C.BVSrem nrep r1 r2 -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (NatRepr nrep)
             , toRegisterOperand cache r1
             , toRegisterOperand cache r2
             ]
    C.BVUle nrep r1 r2 -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (NatRepr nrep)
             , toRegisterOperand cache r1
             , toRegisterOperand cache r2
             ]
    C.BVUlt nrep r1 r2 -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (NatRepr nrep)
             , toRegisterOperand cache r1
             , toRegisterOperand cache r2
             ]
    C.BVSle nrep r1 r2 -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (NatRepr nrep)
             , toRegisterOperand cache r1
             , toRegisterOperand cache r2
             ]
    C.BVSlt nrep r1 r2 -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (NatRepr nrep)
             , toRegisterOperand cache r1
             , toRegisterOperand cache r2
             ]
    C.BVCarry nrep r1 r2 -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (NatRepr nrep)
             , toRegisterOperand cache r1
             , toRegisterOperand cache r2
             ]
    C.BVSCarry nrep r1 r2 -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (NatRepr nrep)
             , toRegisterOperand cache r1
             , toRegisterOperand cache r2
             ]
    C.BVSBorrow nrep r1 r2 -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (NatRepr nrep)
             , toRegisterOperand cache r1
             , toRegisterOperand cache r2
             ]
    C.BVShl nrep r1 r2 -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (NatRepr nrep)
             , toRegisterOperand cache r1
             , toRegisterOperand cache r2
             ]
    C.BVLshr nrep r1 r2 -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (NatRepr nrep)
             , toRegisterOperand cache r1
             , toRegisterOperand cache r2
             ]
    C.BVAshr nrep r1 r2 -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (NatRepr nrep)
             , toRegisterOperand cache r1
             , toRegisterOperand cache r2
             ]
    C.BoolToBV nr r -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (NatRepr nr)
             , toRegisterOperand cache r
             ]
    C.BvToInteger nr r -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (NatRepr nr)
             , toRegisterOperand cache r
             ]
    C.SbvToInteger nr r -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (NatRepr nr)
             , toRegisterOperand cache r
             ]
    C.BvToNat nr r -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (NatRepr nr)
             , toRegisterOperand cache r
             ]
    C.BVNonzero nr r -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (NatRepr nr)
             , toRegisterOperand cache r
             ]
    C.BVUMin nr r1 r2 -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (NatRepr nr)
                        , toRegisterOperand cache r1
                        , toRegisterOperand cache r2
                        ]
    C.BVUMax nr r1 r2 -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (NatRepr nr)
                        , toRegisterOperand cache r1
                        , toRegisterOperand cache r2
                        ]
    C.BVSMin nr r1 r2 -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (NatRepr nr)
                        , toRegisterOperand cache r1
                        , toRegisterOperand cache r2
                        ]
    C.BVSMax nr r1 r2 -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (NatRepr nr)
                        , toRegisterOperand cache r1
                        , toRegisterOperand cache r2
                        ]

    C.EmptyWordMap nr btr -> do
      n1 <- PN.freshNonce ng
      n2 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (NatRepr nr)
             , CrucibleOperand n2 (BaseTypeRepr btr)
             ]
    C.InsertWordMap nr btr r1 r2 r3 -> do
      n1 <- PN.freshNonce ng
      n2 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (NatRepr nr)
             , CrucibleOperand n2 (BaseTypeRepr btr)
             , toRegisterOperand cache r1
             , toRegisterOperand cache r2
             , toRegisterOperand cache r3
             ]
    C.LookupWordMap btr r1 r2 -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (BaseTypeRepr btr)
             , toRegisterOperand cache r1
             , toRegisterOperand cache r2
             ]
    C.LookupWordMapWithDefault btr r1 r2 r3 -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (BaseTypeRepr btr)
             , toRegisterOperand cache r1
             , toRegisterOperand cache r2
             , toRegisterOperand cache r3
             ]

    C.InjectVariant crepr idx r -> do
      n1 <- PN.freshNonce ng
      n2 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (CtxRepr crepr)
             , CrucibleOperand n2 (Index idx)
             , toRegisterOperand cache r
             ]
    C.ProjectVariant crepr idx r -> do
      n1 <- PN.freshNonce ng
      n2 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (CtxRepr crepr)
             , CrucibleOperand n2 (Index idx)
             , toRegisterOperand cache r
             ]

    C.MkStruct crep regs -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromItemList [ OL.Item (CrucibleOperand n1 (CtxRepr crep))
                            , OL.Delimited OL.Brackets (OL.fromList (FC.toListFC (toRegisterOperand cache) regs))
                            ]
    C.GetStruct r idx trep -> do
      n1 <- PN.freshNonce ng
      n2 <- PN.freshNonce ng
      return $ OL.fromList [ toRegisterOperand cache r
             , CrucibleOperand n1 (Index idx)
             , CrucibleOperand n2 (TypeRepr trep)
             ]
    C.SetStruct crep r1 idx r2 -> do
      n1 <- PN.freshNonce ng
      n2 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (CtxRepr crep)
             , toRegisterOperand cache r1
             , CrucibleOperand n2 (Index idx)
             , toRegisterOperand cache r2
             ]

    C.EmptyStringMap trep -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (TypeRepr trep) ]
    C.LookupStringMapEntry trep r1 r2 -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (TypeRepr trep)
             , toRegisterOperand cache r1
             , toRegisterOperand cache r2
             ]
    C.InsertStringMapEntry trep r1 r2 r3 -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (TypeRepr trep)
             , toRegisterOperand cache r1
             , toRegisterOperand cache r2
             , toRegisterOperand cache r3
             ]

    C.StringLit sl -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (StringLiteral sl) ]
    C.StringEmpty srep -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (StringInfoRepr srep) ]
    C.StringConcat srep r1 r2 -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (StringInfoRepr srep)
                        , toRegisterOperand cache r1
                        , toRegisterOperand cache r2
                        ]
    C.StringLength r1 -> do
      return $ OL.fromList [ toRegisterOperand cache r1 ]
    C.StringContains r1 r2 -> do
      return $ OL.fromList [ toRegisterOperand cache r1
                        , toRegisterOperand cache r2
                        ]
    C.StringIsPrefixOf r1 r2 -> do
      return $ OL.fromList [ toRegisterOperand cache r1
                        , toRegisterOperand cache r2
                        ]
    C.StringIsSuffixOf r1 r2 -> do
      return $ OL.fromList [ toRegisterOperand cache r1
                        , toRegisterOperand cache r2
                        ]
    C.StringIndexOf r1 r2 r3 -> do
      return $ OL.fromList [ toRegisterOperand cache r1
                        , toRegisterOperand cache r2
                        , toRegisterOperand cache r3
                        ]
    C.StringSubstring srep r1 r2 r3 -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (StringInfoRepr srep)
                        , toRegisterOperand cache r1
                        , toRegisterOperand cache r2
                        , toRegisterOperand cache r3
                        ]
    C.ShowValue btrep r -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (BaseTypeRepr btrep)
             , toRegisterOperand cache r
             ]
    C.ShowFloat frep r1 -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (FloatInfoRepr frep)
                        , toRegisterOperand cache r1
                        ]


    C.SymArrayLookup btr r bts -> do
      n1 <- PN.freshNonce ng
      bts' <- sequence $ FC.toListFC (\bt -> do
                      n <- PN.freshNonce ng
                      return (CrucibleOperand n (BaseTerm bt))) bts
      return $ OL.fromItemList [ OL.Item (CrucibleOperand n1 (BaseTypeRepr btr))
                            , OL.Item (toRegisterOperand cache r)
                            , OL.Delimited OL.Brackets (OL.fromList bts')
                            ]
    C.SymArrayUpdate btr r1 bts r2 -> do
      n1 <- PN.freshNonce ng
      bts' <- sequence $ FC.toListFC (\bt -> do
                      n <- PN.freshNonce ng
                      return (CrucibleOperand n (BaseTerm bt))) bts
      return $ OL.fromItemList [ OL.Item (CrucibleOperand n1 (BaseTypeRepr btr))
                            , OL.Item (toRegisterOperand cache r1)
                            , OL.Delimited OL.Brackets (OL.fromList bts')
                            , OL.Item (toRegisterOperand cache r2)
                            ]
    C.IsConcrete btrep r -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (BaseTypeRepr btrep)
             , toRegisterOperand cache r
             ]
    C.ReferenceEq trep r1 r2 -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [ CrucibleOperand n1 (TypeRepr trep)
             , toRegisterOperand cache r1
             , toRegisterOperand cache r2
             ]

    C.ExtensionApp exprExt -> OL.fromList <$> extensionExprOperands cache ng exprExt


instance NFData (Address (Crucible arch) s) where
  rnf (CrucibleAddress a) =
    case a of
      FunctionAddr !_w -> ()
      BlockAddr !_fa !_idx -> ()
      InstructionAddr !_ba !_idx -> ()

instance Eq (Address (Crucible arch) s) where
  CrucibleAddress a1 == CrucibleAddress a2 = isJust (testEquality a1 a2)

instance Ord (Address (Crucible arch) s) where
  compare (CrucibleAddress a1) (CrucibleAddress a2) = toOrdering (compareF a1 a2)

instance Show (Address (Crucible arch) s) where
  show (CrucibleAddress a) = show a

instance NFData (Instruction (Crucible arch) s) where
  rnf _i = ()

instance NFData (Operand (Crucible arch) s) where
  rnf _ = ()

instance Show (Addr tp) where
  show a =
    case a of
      FunctionAddr w -> printf "0x%x" w
      BlockAddr fa idx -> printf "%s[%s]" (show fa) (show idx)
      InstructionAddr ba idx -> printf "%s@%s" (show ba) (show idx)


$(return [])

instance TestEquality Addr where
  testEquality = $(PTH.structuralTypeEquality [t| Addr |]
                   [(PTH.TypeApp (PTH.ConType [t| Addr |]) PTH.AnyType, [|testEquality|])])

instance OrdF Addr where
  compareF = $(PTH.structuralTypeOrd [t| Addr |]
               [(PTH.TypeApp (PTH.ConType [t| Addr |]) PTH.AnyType, [|compareF|])])
