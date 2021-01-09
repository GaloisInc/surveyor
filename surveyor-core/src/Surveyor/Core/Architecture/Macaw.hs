{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans -fprint-explicit-kinds -fprint-explicit-foralls #-}
module Surveyor.Core.Architecture.Macaw (
  Macaw,
  macawForBlocks,
  MacawException(..)
  ) where

import           Control.DeepSeq ( NFData(rnf), deepseq )
import qualified Control.Exception as X
import           Control.Lens ( (^.) )
import qualified Data.Foldable as F
import qualified Data.IORef as IOR
import           Data.Kind ( Type )
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
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import           Data.Word ( Word64 )
import qualified Renovate as R
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.String as PPS
import           Text.Read ( readMaybe )

import           Surveyor.Core.Architecture.Class
import           Surveyor.Core.IRRepr ( Macaw )
import qualified Surveyor.Core.OperandList as OL
import qualified Surveyor.Core.Panic as SCP

macawForBlocks :: forall arch s
                . (MC.MemWidth (MC.ArchAddrWidth arch), Ord (Address arch s), MC.ArchConstraints arch, Show (Address arch s))
               => (MC.MemAddr (MC.ArchAddrWidth arch) -> Address arch s)
               -> PN.NonceGenerator IO s
               -> R.BlockInfo arch
               -> R.ConcreteAddress arch
               -> [(MC.ArchSegmentOff arch, Block arch s)]
               -> IO ([Block (Macaw arch) s], BlockMapping arch (Macaw arch) s)
macawForBlocks toArchAddr nonceGen binfo faddr blocks = do
  case M.lookup faddr (R.biFunctions binfo) of
    Nothing -> X.throwIO (NoMacawFunctionFor faddr)
    Just (_, Some dfi) -> do
      let fh :: FunctionHandle (Macaw arch) s
          fh = FunctionHandle { fhAddress = MacawAddress (R.absoluteAddress faddr)
                              , fhName = T.pack (show faddr)
                              }
      blks <- mapM (toMacawBlock nonceGen dfi fh) blocks
      let (_, macawInsnIdx) = F.foldl' (buildMacawInstIndex toArchAddr) (Nothing, M.empty) (concatMap (blockInstructions . snd) blks)
      let bm = BlockMapping { blockMapping = toBlockMap blks
                            , irToBaseAddrs = macawInsnIdx
                            , baseToIRAddrs = flipAddressMap macawInsnIdx
                            }
      return (map snd blks, bm)
  where
    toBlockMap pairs = M.fromList [ (blockAddress origBlock, (origBlock, mblock))
                                  | (origBlock, mblock) <- pairs
                                  ]

buildMacawInstIndex :: (Ord (Address arch s), MC.MemWidth (MC.ArchAddrWidth arch))
                    => (MC.MemAddr (MC.ArchAddrWidth arch) -> Address arch s)
                    -> (Maybe (Address arch s), M.Map (Address (Macaw arch) s) (S.Set (Address arch s)))
                    -> (Address (Macaw arch) s, Instruction (Macaw arch) s)
                    -> (Maybe (Address arch s), M.Map (Address (Macaw arch) s) (S.Set (Address arch s)))
buildMacawInstIndex toArchAddr acc@(mCurrentInsnAddr, m) (iaddr, i) =
  case i of
    MacawTermStmt _ _
      | Just cia <- mCurrentInsnAddr ->
        (mCurrentInsnAddr, M.insertWith S.union iaddr (S.singleton cia) m)
      | otherwise -> acc
    MacawInstructionStart baddr off _stmt _ ->
      let cia' = toArchAddr (MC.incAddr (fromIntegral off) (MC.segoffAddr baddr))
      in (Just cia', m)
    MacawStmt s _ _ ->
      case s of
        MC.Comment {} -> acc
        _ | Just cia <- mCurrentInsnAddr ->
              (mCurrentInsnAddr, M.insertWith S.union iaddr (S.singleton cia) m)
          | otherwise -> acc

flipAddressMap :: (Ord (Address arch s))
               => M.Map (Address (Macaw arch) s) (S.Set (Address arch s))
               -> M.Map (Address arch s) (S.Set (Address (Macaw arch) s))
flipAddressMap = foldr doFlip M.empty . M.toList
  where
    doFlip (macawAddr, mcAddrs) m = F.foldr (addMachineAddrs macawAddr) m mcAddrs
    addMachineAddrs macawAddr machineAddr = M.insertWith S.union machineAddr (S.singleton macawAddr)

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
      -- let stmts = MC.blockStatementList pb
      nc <- IOR.newIORef MapF.empty
      let cache = NonceCache nc
      insns <- buildInsnList cache 0 (MC.pblockStmts pb) (MC.pblockTermStmt pb)
      let mb = Block { blockAddress = BlockNumber (MC.pblockAddr pb)
                     , blockInstructions = insns
                     , blockFunction = fh
                     }
      return (b, mb)
    where
      -- We use a cache to memoize the translations of Macaw Values (so that we
      -- can identify definitions and uses)
      buildInsnList cache insnIdx nonterms term =
        case nonterms of
          [] -> do
            ops <- macawTermStmtOperands cache nonceGen term
            return [(InstructionNumber insnIdx, MacawTermStmt term ops)]
          s:stmts ->
            case s of
              MC.InstructionStart off _ -> do
                ops <- macawStmtOperands cache nonceGen s
                rest <- buildInsnList cache (insnIdx + 1) stmts term
                return ((InstructionNumber insnIdx, MacawInstructionStart baddr off s ops) : rest)
              _ -> do
                mbv <- macawBoundValue cache nonceGen s
                ops <- macawStmtOperands cache nonceGen s
                rest <- buildInsnList cache (insnIdx + 1) stmts term
                return ((InstructionNumber insnIdx, MacawStmt s mbv ops) : rest)

data NonceCache ids s where
  NonceCache :: forall (oldNonce :: MT.Type -> Type) (newNonce :: MT.Type -> Type) ids s
              . ( oldNonce ~ PN.Nonce ids
                , newNonce ~ PN.Nonce s
                )
             => IOR.IORef (MapF.MapF oldNonce newNonce)
             -> NonceCache ids s

-- | Get the fresh nonce we have allocated for this value
--
-- If there was already a nonce assigned to this value before, return that
-- nonce.  Otherwise, allocate a fresh one.
getValueNonce :: forall arch ids s tp
               . NonceCache ids s
              -> PN.NonceGenerator IO s
              -> MC.Value arch ids tp
              -> IO (PN.Nonce s tp)
getValueNonce cache nonceGen val = do
  case val of
    MC.AssignedValue asgn -> do
      let MC.AssignId aid = MC.assignId asgn
      getCachedNonce cache nonceGen aid
    _ -> PN.freshNonce nonceGen

getCachedNonce :: forall ids s (tp :: MT.Type)
                . NonceCache ids s
               -> PN.NonceGenerator IO s
               -> PN.Nonce ids tp
               -> IO (PN.Nonce s tp)
getCachedNonce (NonceCache nc) nonceGen n = do
  m <- IOR.readIORef nc
  case MapF.lookup n m of
    Just n' -> return n'
    Nothing -> do
      n' <- PN.freshNonce nonceGen
      IOR.writeIORef nc (MapF.insert n n' m)
      return n'

toValueCached :: NonceCache ids s
              -> PN.NonceGenerator IO s
              -> MC.Value arch ids tp
              -> IO (Operand (Macaw arch) s)
toValueCached cache ng v = do
  n <- getValueNonce cache ng v
  return (MacawOperand n (Value v))

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
  data Instruction (Macaw arch) s = forall ids . MacawStmt (MC.Stmt arch ids) (Maybe (Operand (Macaw arch) s)) (OL.OperandList (Operand (Macaw arch) s))
                                  | forall ids . MacawInstructionStart (MC.ArchSegmentOff arch) (MC.ArchAddrWord arch) (MC.Stmt arch ids) (OL.OperandList (Operand (Macaw arch) s))
                                  | forall ids . MacawTermStmt (MC.ParsedTermStmt arch ids) (OL.OperandList (Operand (Macaw arch) s))
  data Address (Macaw arch) s = MacawAddress (MC.ArchAddrWord arch)
                              | BlockNumber (MC.ArchSegmentOff arch)
                              | InstructionNumber Int
  data Operand (Macaw arch) s = forall tp . MacawOperand (PN.Nonce s tp) (MacawOperand arch s)
  data Opcode (Macaw arch) s = MacawOpcode (MacawOpcode arch s)

  boundValue (MacawStmt _stmt mbv _) = mbv
  boundValue (MacawInstructionStart {}) = Nothing
  boundValue (MacawTermStmt _ _) = Nothing

  operands (MacawStmt _stmt _ ops) = ops
  operands (MacawInstructionStart _ _ _ ops) = ops
  operands (MacawTermStmt _stmt ops) = ops
  opcode (MacawStmt stmt _ _) = macawStmtOpcode stmt
  opcode (MacawInstructionStart _ _ stmt _) = macawStmtOpcode stmt
  opcode (MacawTermStmt stmt _) = macawTermStmtOpcode stmt
  prettyInstruction addr (MacawStmt stmt _ _) =
    case addr of
      MacawAddress a -> MC.ppStmt (\off -> PP.viaShow (a + off)) stmt
      BlockNumber _ -> MC.ppStmt (\off -> PP.viaShow off) stmt
      InstructionNumber i -> PP.pretty "i" <> PP.pretty i
  prettyInstruction _ (MacawInstructionStart _ _ stmt _) = PP.viaShow stmt
  prettyInstruction _ (MacawTermStmt stmt _) = PP.viaShow stmt
  prettyAddress a =
    case a of
      MacawAddress addr -> PP.viaShow addr
      BlockNumber n -> PP.pretty "b" <> PP.viaShow n
      InstructionNumber n -> PP.pretty "i" <> PP.viaShow n
  prettyOpcode (MacawOpcode opc) = macawPrettyOpcode opc
  prettyOperand _addr (MacawOperand _ opr) =
    macawPrettyOperand opr
  parseAddress t = (MacawAddress . fromIntegral) <$> ((readMaybe t) :: Maybe Word64)

  rawRepr = Nothing
  showInstructionAddresses _ = False
  operandSelectable (MacawOperand _ o) =
    case o of
      Binder {} -> True
      AddressLiteral {} -> True
      Value {} -> True
      Addr {} -> True
      SegmentOffset {} -> True
      BlockId {} -> True
      Relocation {} -> True
      -- FIXME: Break this down
      JumpTable {} -> False
      -- FIXME: Break this down
      RegState {} -> False
      -- FIXME: Break this down
      StateUpdate {} -> False
      -- Not selectable
      TypeRepr {} -> False
      TypeReprs {} -> False
      Index {} -> False
      NatRepr {} -> False
      MemRepr {} -> False
      CommentText {} -> False

deriving instance Eq (Address (Macaw arch) s)
deriving instance Ord (Address (Macaw arch) s)
instance (MacawConstraints arch s) => Show (Address (Macaw arch) s) where
  show a = PPS.renderString (PP.layoutCompact (prettyAddress a))
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
      MacawInstructionStart !_ !_ !_ ops -> ops `deepseq` ()

instance NFData (Operand (Macaw arch) s) where
  rnf (MacawOperand !_nonce mo) = mo `deepseq` ()

macawBoundValue :: NonceCache ids s -> PN.NonceGenerator IO s -> MC.Stmt arch ids -> IO (Maybe (Operand (Macaw arch) s))
macawBoundValue cache ng stmt =
  case stmt of
    MC.AssignStmt asgn -> do
      let MC.AssignId assignNonce = MC.assignId asgn
      n1 <- getCachedNonce cache ng assignNonce
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
    MC.CondWriteMem {} -> MacawOpcode CondWriteMem
    MC.InstructionStart {} -> MacawOpcode Comment

macawTermStmtOpcode :: (MC.MemWidth (MC.ArchAddrWidth arch)) => MC.ParsedTermStmt arch ids -> Opcode (Macaw arch) s
macawTermStmtOpcode stmt =
  case stmt of
    MC.ParsedCall _ Nothing -> MacawOpcode TailCall
    MC.ParsedCall _ (Just _) -> MacawOpcode Call
    MC.ParsedJump {} -> MacawOpcode Jump
    MC.ParsedBranch {} -> MacawOpcode Branch
    MC.ParsedLookupTable {} -> MacawOpcode IndirectJumpTable
    MC.ParsedReturn {} -> MacawOpcode Return
    MC.ParsedTranslateError t -> MacawOpcode (TranslateError t)
    MC.ClassifyFailure {} -> MacawOpcode ClassifyFailure
    MC.ParsedArchTermStmt t _regs _next -> MacawOpcode (ArchTermStmt t)
    MC.PLTStub _regs addr _relo ->
      SCP.panic "Macaw Translation" [ "Unhandled PLT stub"
                                    , show addr
                                    ]

-- FIXME: Nonces need to be memoized (i.e., the same operand appearing in
-- multiple locations should get the same global nonce)
macawTermStmtOperands :: NonceCache ids s
                      -> PN.NonceGenerator IO s
                      -> MC.ParsedTermStmt arch ids
                      -> IO (OL.OperandList (Operand (Macaw arch) s))
macawTermStmtOperands cache ng stmt =
  case stmt of
    MC.PLTStub regs next relo -> do
      n1 <- PN.freshNonce ng
      n2 <- PN.freshNonce ng
      n3 <- PN.freshNonce ng
      return $ OL.fromList [ MacawOperand n1 (StateUpdate regs)
                        , MacawOperand n2 (SegmentOffset next)
                        , MacawOperand n3 (Relocation relo)
                        ]
    MC.ParsedCall regs Nothing -> do
      n1 <- PN.freshNonce ng
      -- FIXME: We can actually break out all of the registers as individual arguments
      return $ OL.fromList [MacawOperand n1 (RegState regs)]
    MC.ParsedCall regs (Just next) -> do
      n1 <- PN.freshNonce ng
      n2 <- PN.freshNonce ng
      return $ OL.fromList [MacawOperand n1 (RegState regs), MacawOperand n2 (SegmentOffset next)]
    MC.ParsedJump regs next -> do
      n1 <- PN.freshNonce ng
      n2 <- PN.freshNonce ng
      return $ OL.fromList [MacawOperand n1 (RegState regs), MacawOperand n2 (SegmentOffset next)]
    MC.ParsedBranch regs cond addr1 addr2 -> do
      n1 <- PN.freshNonce ng
      n2 <- PN.freshNonce ng
      n3 <- PN.freshNonce ng
      cond' <- toValueCached cache ng cond
      return $ OL.fromList [ MacawOperand n1 (RegState regs)
                        , cond'
                        , MacawOperand n2 (SegmentOffset addr1)
                        , MacawOperand n3 (SegmentOffset addr2)
                        ]
    MC.ParsedLookupTable regs idx tbl -> do
      n1 <- PN.freshNonce ng
      idx' <- toValueCached cache ng idx
      n3 <- PN.freshNonce ng
      return $ OL.fromList [ MacawOperand n1 (RegState regs)
             , idx'
             , MacawOperand n3 (JumpTable tbl)
             ]
    MC.ParsedReturn regs -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [MacawOperand n1 (RegState regs)]
    MC.ParsedArchTermStmt _ regs Nothing -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [MacawOperand n1 (RegState regs)]
    MC.ParsedArchTermStmt _ regs (Just next) -> do
      n1 <- PN.freshNonce ng
      n2 <- PN.freshNonce ng
      return $ OL.fromList [MacawOperand n1 (RegState regs), MacawOperand n2 (SegmentOffset next)]
    MC.ParsedTranslateError {} -> return $ OL.fromList []
    MC.ClassifyFailure regs _ -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [MacawOperand n1 (RegState regs)]

macawStmtOperands :: NonceCache ids s -> PN.NonceGenerator IO s -> MC.Stmt arch ids -> IO (OL.OperandList (Operand (Macaw arch) s))
macawStmtOperands cache ng stmt =
  case stmt of
    MC.Comment t -> do
      n1 <- PN.freshNonce ng
      return $ OL.fromList [MacawOperand n1 (CommentText t)]
    MC.InstructionStart addr t -> do
      n1 <- PN.freshNonce ng
      n2 <- PN.freshNonce ng
      return $ OL.fromList [MacawOperand n1 (AddressLiteral addr), MacawOperand n2 (CommentText t)]
    MC.WriteMem addr memRep v -> do
      addr' <- toValueCached cache ng addr
      n2 <- PN.freshNonce ng
      v' <- toValueCached cache ng v
      return $ OL.fromList [ addr'
                        , MacawOperand n2 (MemRepr memRep)
                        , v'
                        ]
    MC.CondWriteMem cond addr memRep v -> do
      n1 <- PN.freshNonce ng
      cond' <- toValueCached cache ng cond
      addr' <- toValueCached cache ng addr
      v' <- toValueCached cache ng v
      return $ OL.fromList [ cond'
                        , addr'
                        , MacawOperand n1 (MemRepr memRep)
                        , v'
                        ]
    MC.ArchState addr upd -> do
      n1 <- PN.freshNonce ng
      n2 <- PN.freshNonce ng
      return $ OL.fromList [MacawOperand n1 (Addr addr), MacawOperand n2 (StateUpdate upd)]
  --   -- FIXME: Need a helper to deconstruct these on a per-arch basis
    MC.ExecArchStmt {} -> return (OL.fromList [])
    MC.AssignStmt asgn -> do
      case MC.assignRhs asgn of
        -- FIXME: Need a helper to dissect arch-specific functions
        MC.EvalArchFn {} -> return (OL.fromList [])
        MC.SetUndefined rep -> do
          n1 <- PN.freshNonce ng
          return $ OL.fromList [MacawOperand n1 (TypeRepr rep)]
        MC.ReadMem addr memRep -> do
          addr' <- toValueCached cache ng addr
          n2 <- PN.freshNonce ng
          return $ OL.fromList [addr', MacawOperand n2 (MemRepr memRep)]
        MC.CondReadMem memRep c addr def -> do
          n1 <- PN.freshNonce ng
          c' <- toValueCached cache ng c
          addr' <- toValueCached cache ng addr
          def' <- toValueCached cache ng def
          return $ OL.fromList [ MacawOperand n1 (MemRepr memRep)
                            , c'
                            , addr'
                            , def'
                            ]
        MC.EvalApp app ->
          case app of
            MC.Eq v1 v2 -> do
              v1' <- toValueCached cache ng v1
              v2' <- toValueCached cache ng v2
              return $ OL.fromList [v1', v2']
            MC.Mux rep c v1 v2 -> do
              n1 <- PN.freshNonce ng
              c' <- toValueCached cache ng c
              v1' <- toValueCached cache ng v1
              v2' <- toValueCached cache ng v2
              return $ OL.fromList [ MacawOperand n1 (TypeRepr rep)
                                , c'
                                , v1'
                                , v2'
                                ]
            MC.MkTuple tps vs -> do
              n1 <- PN.freshNonce ng
              let vsM = FC.toListFC (toValueCached cache ng) vs
              vs' <- sequence vsM
              return $! OL.fromList ( MacawOperand n1 (TypeReprs tps)
                                    : vs'
                                    )
            MC.TupleField tps v ix -> do
              n1 <- PN.freshNonce ng
              v' <- toValueCached cache ng v
              n3 <- PN.freshNonce ng
              return $ OL.fromList [MacawOperand n1 (TypeReprs tps), v', MacawOperand n3 (Index ix)]
            MC.AndApp v1 v2 -> do
              v1' <- toValueCached cache ng v1
              v2' <- toValueCached cache ng v2
              return $ OL.fromList [v1', v2']
            MC.OrApp v1 v2 -> do
              v1' <- toValueCached cache ng v1
              v2' <- toValueCached cache ng v2
              return $ OL.fromList [v1', v2']
            MC.NotApp v -> do
              v' <- toValueCached cache ng v
              return $ OL.fromList [v']
            MC.XorApp v1 v2 -> do
              v1' <- toValueCached cache ng v1
              v2' <- toValueCached cache ng v2
              return $ OL.fromList [v1', v2']
            MC.Trunc v tp -> do
              v' <- toValueCached cache ng v
              n2 <- PN.freshNonce ng
              return $ OL.fromList [v', MacawOperand n2 (NatRepr tp)]
            MC.SExt v tp -> do
              v' <- toValueCached cache ng v
              n2 <- PN.freshNonce ng
              return $ OL.fromList [v', MacawOperand n2 (NatRepr tp)]
            MC.UExt v tp -> do
              v' <- toValueCached cache ng v
              n2 <- PN.freshNonce ng
              return $ OL.fromList [v', MacawOperand n2 (NatRepr tp)]
            MC.BVAdd tp v1 v2 -> do
              n1 <- PN.freshNonce ng
              v1' <- toValueCached cache ng v1
              v2' <- toValueCached cache ng v2
              return $ OL.fromList [MacawOperand n1 (NatRepr tp), v1', v2']
            MC.BVAdc tp v1 v2 c -> do
              n1 <- PN.freshNonce ng
              v1' <- toValueCached cache ng v1
              v2' <- toValueCached cache ng v2
              c' <- toValueCached cache ng c
              return $ OL.fromList [ MacawOperand n1 (NatRepr tp)
                                , v1'
                                , v2'
                                , c'
                                ]
            MC.BVSub tp v1 v2 -> do
              n1 <- PN.freshNonce ng
              v1' <- toValueCached cache ng v1
              v2' <- toValueCached cache ng v2
              return $ OL.fromList [MacawOperand n1 (NatRepr tp), v1', v2']
            MC.BVSbb tp v1 v2 c -> do
              n1 <- PN.freshNonce ng
              v1' <- toValueCached cache ng v1
              v2' <- toValueCached cache ng v2
              c' <- toValueCached cache ng c
              return $ OL.fromList [ MacawOperand n1 (NatRepr tp)
                                , v1'
                                , v2'
                                , c'
                                ]
            MC.BVMul tp v1 v2 -> do
              n1 <- PN.freshNonce ng
              v1' <- toValueCached cache ng v1
              v2' <- toValueCached cache ng v2
              return $ OL.fromList [MacawOperand n1 (NatRepr tp), v1', v2']
            MC.BVUnsignedLe v1 v2 -> do
              v1' <- toValueCached cache ng v1
              v2' <- toValueCached cache ng v2
              return $ OL.fromList [v1', v2']
            MC.BVUnsignedLt v1 v2 -> do
              v1' <- toValueCached cache ng v1
              v2' <- toValueCached cache ng v2
              return $ OL.fromList [v1', v2']
            MC.BVSignedLe v1 v2 -> do
              v1' <- toValueCached cache ng v1
              v2' <- toValueCached cache ng v2
              return $ OL.fromList [v1', v2']
            MC.BVSignedLt v1 v2 -> do
              v1' <- toValueCached cache ng v1
              v2' <- toValueCached cache ng v2
              return $ OL.fromList [v1', v2']
            MC.BVTestBit v1 v2 -> do
              v1' <- toValueCached cache ng v1
              v2' <- toValueCached cache ng v2
              return $ OL.fromList [v1', v2']
            MC.BVComplement tp v -> do
              n1 <- PN.freshNonce ng
              v' <- toValueCached cache ng v
              return $ OL.fromList [MacawOperand n1 (NatRepr tp), v']
            MC.BVAnd tp v1 v2 -> do
              n1 <- PN.freshNonce ng
              v1' <- toValueCached cache ng v1
              v2' <- toValueCached cache ng v2
              return $ OL.fromList [MacawOperand n1 (NatRepr tp), v1', v2']
            MC.BVOr tp v1 v2 -> do
              n1 <- PN.freshNonce ng
              v1' <- toValueCached cache ng v1
              v2' <- toValueCached cache ng v2
              return $ OL.fromList [MacawOperand n1 (NatRepr tp), v1', v2']
            MC.BVXor tp v1 v2 -> do
              n1 <- PN.freshNonce ng
              v1' <- toValueCached cache ng v1
              v2' <- toValueCached cache ng v2
              return $ OL.fromList [MacawOperand n1 (NatRepr tp), v1', v2']
            MC.BVShl tp v1 v2 -> do
              n1 <- PN.freshNonce ng
              v1' <- toValueCached cache ng v1
              v2' <- toValueCached cache ng v2
              return $ OL.fromList [MacawOperand n1 (NatRepr tp), v1', v2']
            MC.BVShr tp v1 v2 -> do
              n1 <- PN.freshNonce ng
              v1' <- toValueCached cache ng v1
              v2' <- toValueCached cache ng v2
              return $ OL.fromList [MacawOperand n1 (NatRepr tp), v1', v2']
            MC.BVSar tp v1 v2 -> do
              n1 <- PN.freshNonce ng
              v1' <- toValueCached cache ng v1
              v2' <- toValueCached cache ng v2
              return $ OL.fromList [MacawOperand n1 (NatRepr tp), v1', v2']
            MC.UadcOverflows v1 v2 c -> do
              v1' <- toValueCached cache ng v1
              v2' <- toValueCached cache ng v2
              c' <- toValueCached cache ng c
              return $ OL.fromList [v1', v2', c']
            MC.SadcOverflows v1 v2 c -> do
              v1' <- toValueCached cache ng v1
              v2' <- toValueCached cache ng v2
              c' <- toValueCached cache ng c
              return $ OL.fromList [v1', v2', c']
            MC.UsbbOverflows v1 v2 c -> do
              v1' <- toValueCached cache ng v1
              v2' <- toValueCached cache ng v2
              c' <- toValueCached cache ng c
              return $ OL.fromList [v1', v2', c']
            MC.SsbbOverflows v1 v2 c -> do
              v1' <- toValueCached cache ng v1
              v2' <- toValueCached cache ng v2
              c' <- toValueCached cache ng c
              return $ OL.fromList [v1', v2', c']
            MC.PopCount tp v -> do
              n1 <- PN.freshNonce ng
              v' <- toValueCached cache ng v
              return $ OL.fromList [MacawOperand n1 (NatRepr tp), v']
            MC.ReverseBytes tp v -> do
              n1 <- PN.freshNonce ng
              v' <- toValueCached cache ng v
              return $ OL.fromList [MacawOperand n1 (NatRepr tp), v']
            MC.Bsf tp v -> do
              n1 <- PN.freshNonce ng
              v' <- toValueCached cache ng v
              return $ OL.fromList [MacawOperand n1 (NatRepr tp), v']
            MC.Bsr tp v -> do
              n1 <- PN.freshNonce ng
              v' <- toValueCached cache ng v
              return $ OL.fromList [MacawOperand n1 (NatRepr tp), v']
            MC.Bitcast v _proof -> do
              v' <- toValueCached cache ng v
              return $ OL.fromList [ v' ]

macawPrettyOpcode :: (MacawConstraints arch s) => MacawOpcode arch s -> PP.Doc ann
macawPrettyOpcode opc =
  case opc of
    Comment -> PP.pretty "#"
    WriteMem -> PP.pretty "write-mem"
    Undefined -> PP.pretty "undefined"
    ReadMem -> PP.pretty "read-mem"
    CondReadMem -> PP.pretty "cond-read-mem"
    CondWriteMem -> PP.pretty "cond-write-mem"
    ArchState -> PP.pretty "arch_state"
    Call -> PP.pretty "call"
    TailCall -> PP.pretty "tailcall"
    Jump -> PP.pretty "jump"
    Branch -> PP.pretty "branch"
    IndirectJumpTable -> PP.pretty "indirect-jump"
    ClassifyFailure -> PP.pretty "classify-failure"
    Return -> PP.pretty "return"
    TranslateError t -> PP.pretty "translate-error" PP.<+> PP.pretty t
    -- FIXME: This isn't very good - we need an arch-specific backend to print
    -- each archfn correctly
    EvalArchFn fn -> PP.pretty (show (MC.ppArchFn (Just . MC.ppValue 0) fn))
    -- FIXME: Same
    ExecArchStmt stmt -> PP.pretty (show (MC.ppArchStmt (MC.ppValue 0) stmt))
    -- FIXME: Same
    ArchTermStmt t -> PP.pretty (show (MC.prettyF t))
    App a ->
      case a of
        MC.Eq {} -> PP.pretty "eq"
        MC.Mux {} -> PP.pretty "mux"
        MC.MkTuple {} -> PP.pretty "mk-tuple"
        MC.TupleField {} -> PP.pretty "tuple"
        MC.AndApp {} -> PP.pretty "andp"
        MC.OrApp {} -> PP.pretty "orp"
        MC.NotApp {} -> PP.pretty "notp"
        MC.XorApp {} -> PP.pretty "xorp"
        MC.Trunc {} -> PP.pretty "trunc"
        MC.SExt {} -> PP.pretty "sext"
        MC.UExt {} -> PP.pretty "uext"
        MC.BVAdd {} -> PP.pretty "bv-add"
        MC.BVAdc {} -> PP.pretty "bv-adc"
        MC.BVSub {} -> PP.pretty "bv-sub"
        MC.BVSbb {} -> PP.pretty "bv-sbb"
        MC.BVMul {} -> PP.pretty "bv-mul"
        MC.BVUnsignedLe {} -> PP.pretty "bv-ule"
        MC.BVUnsignedLt {} -> PP.pretty "bv-ult"
        MC.BVSignedLe {} -> PP.pretty "bv-sle"
        MC.BVSignedLt {} -> PP.pretty "bv-slt"
        MC.BVTestBit {} -> PP.pretty "test_bit"
        MC.BVComplement {} -> PP.pretty "bv-complement"
        MC.BVAnd {} -> PP.pretty "bv-and"
        MC.BVOr {} -> PP.pretty "bv-or"
        MC.BVXor {} -> PP.pretty "bv-xor"
        MC.BVShl {} -> PP.pretty "bv-shl"
        MC.BVShr {} -> PP.pretty "bv-shr"
        MC.BVSar {} -> PP.pretty "bv-sar"
        MC.UadcOverflows {} -> PP.pretty "uadc_overflows"
        MC.SadcOverflows {} -> PP.pretty "sadc_overflows"
        MC.UsbbOverflows {} -> PP.pretty "usbb_overflows"
        MC.SsbbOverflows {} -> PP.pretty "ssbb_overflows"
        MC.PopCount {} -> PP.pretty "popcount"
        MC.ReverseBytes {} -> PP.pretty "reverse_bytes"
        MC.Bsf {} -> PP.pretty "bsf"
        MC.Bsr {} -> PP.pretty "bsr"
        MC.Bitcast {} -> PP.pretty "bitcast"


macawPrettyOperand :: (MacawConstraints arch s) => MacawOperand arch s -> PP.Doc ann
macawPrettyOperand opr =
  case opr of
    Binder aid -> PP.viaShow (MC.ppAssignId aid)
    CommentText t -> PP.pretty t
    AddressLiteral adr -> PP.viaShow adr
    TypeRepr tp -> PP.viaShow tp
    TypeReprs tps -> PP.viaShow (FC.toListFC (\x -> show x) tps)
    Index idx -> PP.viaShow idx
    NatRepr tp -> PP.viaShow tp
    MemRepr tp -> PP.viaShow tp
    Value v -> PP.viaShow v
    Addr a -> PP.viaShow a
    SegmentOffset off -> PP.viaShow off
    JumpTable tbl -> PP.viaShow tbl
    BlockId w -> PP.viaShow w
    StateUpdate m -> prettyStateUpdate m
    RegState rs -> prettyStateUpdate (MC.regStateMap rs)
    Relocation relo -> PP.viaShow relo

prettyStateUpdate :: (ShowF (MC.ArchReg arch), MC.RegisterInfo (MC.ArchReg arch))
                  => MapF.MapF (MC.ArchReg arch) (MC.Value arch ids)
                  -> PP.Doc ann
prettyStateUpdate m =
  PP.hsep $ PP.punctuate PP.comma
  [ PP.pretty (showF reg) <> PP.pretty " -> " <> PP.viaShow (MC.ppValue 0 val)
  | MapF.Pair reg val <- MapF.toList m
  ]

data MacawOpcode arch s where
  Comment :: MacawOpcode arch s
  WriteMem :: MacawOpcode arch s
  Undefined :: MacawOpcode arch s
  ReadMem :: MacawOpcode arch s
  CondReadMem :: MacawOpcode arch s
  CondWriteMem :: MacawOpcode arch s
  App :: MC.App (MC.Value arch ids) tp -> MacawOpcode arch s
  EvalArchFn :: MC.ArchFn arch (MC.Value arch ids) tp -> MacawOpcode arch s
  ExecArchStmt :: MC.ArchStmt arch (MC.Value arch ids) -> MacawOpcode arch s
  ArchState :: MacawOpcode arch s
  Call :: MacawOpcode arch s
  TailCall :: MacawOpcode arch s
  Jump :: MacawOpcode arch s
  Branch :: MacawOpcode arch s
  IndirectJumpTable :: MacawOpcode arch s
  Return :: MacawOpcode arch s
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
  Relocation :: !MC.VersionedSymbol -> MacawOperand arch s

instance NFData (MacawOperand arch s) where
  rnf !_o = ()
