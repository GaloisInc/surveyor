{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
module Surveyor.Core.GraphViz (
  regEntryToGraphViz
  ) where

import qualified Control.Monad.State.Strict as MS
import qualified Data.Foldable as F
import qualified Data.GraphViz as DG
import qualified Data.Map.Strict as Map
import           Data.Maybe ( isJust )
import qualified Data.Parameterized.Classes as PC
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.Nonce as PN
import qualified Data.Parameterized.TraversableFC as FC
import qualified Data.Set as Set
import qualified Lang.Crucible.LLVM.MemModel as CLM
import qualified Lang.Crucible.Simulator.RegMap as LMCR
import qualified Lang.Crucible.Types as LCT
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.Text as PPT
import qualified What4.Expr.Builder as WEB
import qualified What4.Interface as WI
import qualified What4.ProgramLoc as WPL
import qualified What4.Symbol as WS
import qualified What4.Utils.Complex as WUC
import qualified What4.Utils.StringLiteral as WUS

-- | How a sub-term should be rendered in its host term
--
-- This will either be a unique identifier @$n@ or an inlined constant
data Rendering where
  -- | Render the term inline (text contained in the doc)
  --
  -- This is useful to distinguish the top-level node; if the top-level node is
  -- inlined, generate a fake (distinguished) root node for it
  RenderingInline :: PP.Doc () -> Rendering
  -- | Render a reference to the term (contained in the doc)
  Rendering :: PP.Doc () -> Rendering

-- | This is a wrapper around register values so that we can provide 'Eq' and
-- 'Ord' instances for graphviz
data FormulaNode s sym where
  -- | This node type represents 'LCSR.RegValue's that have their own nonce
  NonceNode :: forall s st fs sym (tp :: LCT.BaseType) . (sym ~ WEB.ExprBuilder s st fs) => PN.Nonce s tp -> FormulaNode s sym
  -- | This is a surrogate node for use when the root 'LMCR.RegEntry' does not have a nonce
  --
  -- NOTE: It is important that only one root node be created, as the Eq/Ord
  -- instances play fast and loose with it
  RootNode :: LMCR.RegEntry sym tp -> FormulaNode s sym

instance Eq (FormulaNode s sym) where
  NonceNode n1 == NonceNode n2 = PN.indexValue n1 == PN.indexValue n2
  RootNode {} == RootNode {} = True
  RootNode {} == _ = False
  _ == RootNode {} = False

instance Ord (FormulaNode s sym) where
  compare (NonceNode n1) (NonceNode n2) =
    compare (PN.indexValue n1) (PN.indexValue n2)
  compare RootNode {} RootNode {} = EQ
  compare RootNode {} _ = LT
  compare _ RootNode {} = GT

instance DG.PrintDot (FormulaNode s sym) where
  unqtDot (NonceNode n) = DG.unqtDot (show (PN.indexValue n))
  unqtDot (RootNode _) = DG.unqtDot "root"

data FormulaNodeLabel where
  NormalNodeLabel :: PP.Doc () -> Maybe WPL.ProgramLoc -> FormulaNodeLabel
  RootNodeLabel :: PP.Doc () -> Maybe WPL.ProgramLoc -> FormulaNodeLabel

data FormulaEdgeLabel where
  -- | A normal dependency edge
  DependencyEdge :: FormulaEdgeLabel
  -- | A conditional dependency edge (e.g., in an ITE)
  ConditionalEdge :: Bool -> FormulaEdgeLabel

deriving instance Eq FormulaEdgeLabel
deriving instance Ord FormulaEdgeLabel

data CrucibleNonce s where
  CrucibleNonce :: forall s (tp :: LCT.BaseType) . PN.Nonce s tp -> CrucibleNonce s

instance Eq (CrucibleNonce s) where
  CrucibleNonce n1 == CrucibleNonce n2 = isJust (PC.testEquality n1 n2)

instance Ord (CrucibleNonce s) where
  compare (CrucibleNonce n1) (CrucibleNonce n2) = PC.toOrdering (PC.compareF n1 n2)

data RegEntryBuilderState s sym =
  RegEntryBuilderState { builderEdges :: Map.Map (FormulaNode s sym) (Set.Set (FormulaNode s sym, FormulaEdgeLabel))
                       , builderNodes :: Map.Map (FormulaNode s sym) FormulaNodeLabel
                       , renderCache :: Map.Map (CrucibleNonce s) Rendering
                       }

emptyRegEntryBuilderState :: RegEntryBuilderState s sym
emptyRegEntryBuilderState =
  RegEntryBuilderState { builderEdges = Map.empty
                       , builderNodes = Map.empty
                       , renderCache = Map.empty
                       }

newtype RegEntryBuilder s sym a =
  RegEntryBuilder { runREB :: MS.State (RegEntryBuilderState s sym) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MS.MonadState (RegEntryBuilderState s sym)
           )

runRegEntryBuilder :: RegEntryBuilder s sym () -> RegEntryBuilderState s sym
runRegEntryBuilder act = MS.execState (runREB act) emptyRegEntryBuilderState

inline :: (Monad m) => PP.Doc () -> m Rendering
inline d = return (RenderingInline d)

-- | Recursively traverse the formula structure, adding nodes and edges to the
-- state as we go
traverseFormulaStructure :: (sym ~ WEB.ExprBuilder s st fs)
                         => FormulaNode s sym
                         -- ^ Source of the edge we should make
                         -> LMCR.RegEntry sym tp
                         -- ^ The current node (destination of the edge we make this step, if any)
                         -> RegEntryBuilder s sym Rendering
traverseFormulaStructure predEntry entry =
  case LCT.asBaseType (LMCR.regType entry) of
    LCT.NotBaseType ->
      case LMCR.regType entry of
        LCT.UnitRepr -> do
          -- We don't add an edge to () at all - we render it inline
          inline (PP.pretty "()")
        CLM.LLVMPointerRepr w ->
          case LMCR.regValue entry of
            CLM.LLVMPointer base off -> do
              -- We don't have a unique key for LLVMPointers either, so we
              -- render that node itself inline, but still recursively add edges
              -- from the parent to everything pointed to by the LLVMPointer
              -- constructor.
              baseRender <- traverseFormulaStructure predEntry (LMCR.RegEntry (LCT.baseToType LCT.BaseNatRepr) base)
              offRender <- traverseFormulaStructure predEntry (LMCR.RegEntry (LCT.baseToType (LCT.BaseBVRepr w)) off)
              inline (PP.pretty "LLVMPointer" <> ppArglist [baseRender, offRender])
        _ -> inline (PP.pretty "Unknown Crucible Value")
    LCT.AsBaseType _btp ->
      case LMCR.regValue entry of
        WEB.BoolExpr b _loc -> inline (PP.pretty b)
        WEB.StringExpr sl _loc ->
          case sl of
            WUS.UnicodeLiteral t -> inline (PP.dquotes (PP.pretty t))
            WUS.Char8Literal bs -> inline (PP.pretty (show bs))
            WUS.Char16Literal ws -> inline (PP.pretty (show ws))
        WEB.BoundVarExpr bv -> inline (PP.pretty "$" <> PP.pretty (WS.solverSymbolAsText (WEB.bvarName bv)))
        WEB.NonceAppExpr nae -> do
          let nonce = WEB.nonceExprId nae
          let n = NonceNode nonce
          let regVal = LMCR.regValue entry
          addEdge predEntry n
          cache <- MS.gets renderCache
          case Map.lookup (CrucibleNonce nonce) cache of
            Just r -> return r
            Nothing ->
              case WEB.nonceExprApp nae of
                WEB.Annotation _bt _annotNonce e -> do
                  e' <- traverseFormulaStructure n (regEntry e)
                  bind nonce regVal (PP.pretty "annotated" <> ppArglist [e'])
                WEB.Forall boundVar e -> do
                  e' <- traverseFormulaStructure n (regEntry e)
                  let doc = PP.hcat [ PP.pretty "forall $"
                                    , PP.pretty (WS.solverSymbolAsText (WEB.bvarName boundVar))
                                    , PP.pretty " . "
                                    , ppRender e'
                                    ]
                  bind nonce regVal doc
                WEB.Exists boundVar e -> do
                  e' <- traverseFormulaStructure n (regEntry e)
                  let doc = PP.hcat [ PP.pretty "exists $"
                                    , PP.pretty (WS.solverSymbolAsText (WEB.bvarName boundVar))
                                    , PP.pretty " . "
                                    , ppRender e'
                                    ]
                  bind nonce regVal doc
                WEB.ArrayFromFn sf -> do
                  let doc = PP.hcat [ PP.pretty "arrayFromFn "
                                    , PP.parens (PP.pretty (WS.solverSymbolAsText (WEB.symFnName sf)))
                                    ]
                  bind nonce regVal doc
                WEB.FnApp sf args -> do
                  let es = FC.toListFC (traverseFormulaStructure n . regEntry) args
                  es' <- sequence es
                  let doc = PP.hcat [ PP.pretty (WS.solverSymbolAsText (WEB.symFnName sf))
                                    , ppArglist es'
                                    ]
                  bind nonce regVal doc
                WEB.MapOverArrays sf _reprs vals -> do
                  let es = FC.toListFC (\(WI.ArrayResultWrapper e) -> traverseFormulaStructure n (regEntry e)) vals
                  es' <- sequence es
                  let fnName = WS.solverSymbolAsText (WEB.symFnName sf)
                  let doc = PP.hcat [ PP.pretty "mapOverArrays"
                                    , PP.tupled (PP.pretty fnName : fmap ppRender es')
                                    ]
                  bind nonce regVal doc
                WEB.ArrayTrueOnEntries sf e -> do
                  e' <- traverseFormulaStructure n (regEntry e)
                  let fnName = WS.solverSymbolAsText (WEB.symFnName sf)
                  let doc = PP.hcat [ PP.pretty "arrayTrueOnEntries"
                                    , PP.tupled [PP.pretty fnName, ppRender e']
                                    ]
                  bind nonce regVal doc
        WEB.AppExpr ae -> do
          let nonce = WEB.appExprId ae
          let n = NonceNode nonce
          let regVal = LMCR.regValue entry
          addEdge predEntry n
          cache <- MS.gets renderCache
          case Map.lookup (CrucibleNonce nonce) cache of
            Just r -> return r
            Nothing ->
              case WEB.appExprApp ae of
                WEB.BaseEq _tp e1 e2 -> bindBin nonce regVal n (PP.pretty "eq") e1 e2
                WEB.BaseIte _tp _npred p e2 e3 -> bindTernary nonce regVal n (PP.pretty "ite") p e2 e3
                WEB.NotPred e -> bindUnary nonce regVal n (PP.pretty "notPred") e
                WEB.RealIsInteger e -> bindUnary nonce regVal n (PP.pretty "realIsInteger") e

                WEB.NatDiv e1 e2 -> bindBin nonce regVal n (PP.pretty "natDiv") e1 e2
                WEB.NatMod e1 e2 -> bindBin nonce regVal n (PP.pretty "natMod") e1 e2

                WEB.IntDiv e1 e2 -> bindBin nonce regVal n (PP.pretty "intDiv") e1 e2
                WEB.IntMod e1 e2 -> bindBin nonce regVal n (PP.pretty "intMod") e1 e2

                WEB.RealDiv e1 e2 -> bindBin nonce regVal n (PP.pretty "realDiv") e1 e2
                WEB.RealSqrt e -> bindUnary nonce regVal n (PP.pretty "realSqrt") e
                WEB.RealSin e -> bindUnary nonce regVal n (PP.pretty "realSin") e
                WEB.RealCos e -> bindUnary nonce regVal n (PP.pretty "realCos") e
                WEB.RealATan2 e1 e2 -> bindBin nonce regVal n (PP.pretty "realATan2") e1 e2
                WEB.RealSinh e -> bindUnary nonce regVal n (PP.pretty "realSinh") e
                WEB.RealCosh e -> bindUnary nonce regVal n (PP.pretty "realCosh") e
                WEB.RealExp e -> bindUnary nonce regVal n (PP.pretty "realExp") e
                WEB.RealLog e -> bindUnary nonce regVal n (PP.pretty "realLog") e

                WEB.BVTestBit bitNum e -> do
                  e' <- traverseFormulaStructure n (regEntry e)
                  let doc = PP.hcat [ PP.pretty "bvTestBit"
                                    , PP.tupled [PP.pretty bitNum, ppRender e']
                                    ]
                  bind nonce regVal doc

                WEB.BVSlt e1 e2 -> bindBin nonce regVal n (PP.pretty "bvSlt") e1 e2
                WEB.BVUlt e1 e2 -> bindBin nonce regVal n (PP.pretty "bvUlt") e1 e2
                WEB.BVUdiv _rep e1 e2 -> bindBin nonce regVal n (PP.pretty "bvUdiv") e1 e2
                WEB.BVUrem _rep e1 e2 -> bindBin nonce regVal n (PP.pretty "bvUrem") e1 e2
                WEB.BVSdiv _rep e1 e2 -> bindBin nonce regVal n (PP.pretty "bvSdiv") e1 e2
                WEB.BVSrem _rep e1 e2 -> bindBin nonce regVal n (PP.pretty "bvSrem") e1 e2
                WEB.BVShl _rep e1 e2 -> bindBin nonce regVal n (PP.pretty "bvShl") e1 e2
                WEB.BVLshr _rep e1 e2 -> bindBin nonce regVal n (PP.pretty "bvLshr") e1 e2
                WEB.BVAshr _rep e1 e2 -> bindBin nonce regVal n (PP.pretty "bvAshr") e1 e2
                WEB.BVRol _rep e1 e2 -> bindBin nonce regVal n (PP.pretty "bvRol") e1 e2
                WEB.BVRor _rep e1 e2 -> bindBin nonce regVal n (PP.pretty "bvRor") e1 e2
                WEB.BVZext _rep e -> bindUnary nonce regVal n (PP.pretty "bvZext") e
                WEB.BVSext _rep e -> bindUnary nonce regVal n (PP.pretty "bvSext") e
                WEB.BVPopcount _rep e -> bindUnary nonce regVal n (PP.pretty "bvPopcount") e
                WEB.BVCountTrailingZeros _rep e -> bindUnary nonce regVal n (PP.pretty "bvCtz") e
                WEB.BVCountLeadingZeros _rep e -> bindUnary nonce regVal n (PP.pretty "bvClz") e

                WEB.FloatPZero _rep -> inline (PP.pretty "+0")
                WEB.FloatNZero _rep -> inline (PP.pretty "-0")
                WEB.FloatNaN _rep -> inline (PP.pretty "NaN")
                WEB.FloatPInf _rep -> inline (PP.pretty "+Inf")
                WEB.FloatNInf _rep -> inline (PP.pretty "-Inf")
                WEB.FloatNeg _rep e -> bindUnary nonce regVal n (PP.pretty "floatNeg") e
                WEB.FloatAbs _rep e -> bindUnary nonce regVal n (PP.pretty "floatAbs") e
                WEB.FloatSqrt _rep rm e -> do
                  e' <- traverseFormulaStructure n (regEntry e)
                  let doc = PP.hcat [ PP.pretty "floatSqrt"
                                    , PP.tupled [ PP.viaShow rm, ppRender e']
                                    ]
                  bind nonce regVal doc
                WEB.FloatAdd _rep rm e1 e2 -> bindBinaryFloat nonce regVal n (PP.pretty "floatAdd") rm e1 e2
                WEB.FloatSub _rep rm e1 e2 -> bindBinaryFloat nonce regVal n (PP.pretty "floatSub") rm e1 e2
                WEB.FloatMul _rep rm e1 e2 -> bindBinaryFloat nonce regVal n (PP.pretty "floatMul") rm e1 e2
                WEB.FloatDiv _rep rm e1 e2 -> bindBinaryFloat nonce regVal n (PP.pretty "floatDiv") rm e1 e2
                WEB.FloatRem _rep e1 e2 -> bindBin nonce regVal n (PP.pretty "floatRem") e1 e2
                WEB.FloatMin _rep e1 e2 -> bindBin nonce regVal n (PP.pretty "floatMin") e1 e2
                WEB.FloatMax _rep e1 e2 -> bindBin nonce regVal n (PP.pretty "floatMax") e1 e2
                WEB.FloatFpEq e1 e2 -> bindBin nonce regVal n (PP.pretty "floatFpEq") e1 e2
                WEB.FloatFpNe e1 e2 -> bindBin nonce regVal n (PP.pretty "floatFpNe") e1 e2
                WEB.FloatLe e1 e2 -> bindBin nonce regVal n (PP.pretty "floatLe") e1 e2
                WEB.FloatLt e1 e2 -> bindBin nonce regVal n (PP.pretty "floatLt") e1 e2
                WEB.FloatIsNaN e -> bindUnary nonce regVal n (PP.pretty "floatIsNaN") e
                WEB.FloatIsInf e -> bindUnary nonce regVal n (PP.pretty "floatIsInf") e
                WEB.FloatIsZero e -> bindUnary nonce regVal n (PP.pretty "floatIsZero") e
                WEB.FloatIsPos e -> bindUnary nonce regVal n (PP.pretty "floatIsPos") e
                WEB.FloatIsNeg e -> bindUnary nonce regVal n (PP.pretty "floatIsNeg") e
                WEB.FloatIsSubnorm e -> bindUnary nonce regVal n (PP.pretty "floatIsSubnorm") e
                WEB.FloatIsNorm e -> bindUnary nonce regVal n (PP.pretty "floatIsNorm") e

                WEB.NatToInteger e -> bindUnary nonce regVal n (PP.pretty "natToInteger") e
                WEB.IntegerToNat e -> bindUnary nonce regVal n (PP.pretty "integerToNat") e
                WEB.IntegerToReal e -> bindUnary nonce regVal n (PP.pretty "integerToReal") e
                WEB.RealToInteger e -> bindUnary nonce regVal n (PP.pretty "realToInteger") e
                WEB.BVToNat e -> bindUnary nonce regVal n (PP.pretty "bvToNat") e
                WEB.BVToInteger e -> bindUnary nonce regVal n (PP.pretty "bvToInteger") e
                WEB.SBVToInteger e -> bindUnary nonce regVal n (PP.pretty "sbvToInteger") e

                WEB.RoundReal e -> bindUnary nonce regVal n (PP.pretty "roundReal") e
                WEB.FloorReal e -> bindUnary nonce regVal n (PP.pretty "floorReal") e
                WEB.CeilReal e -> bindUnary nonce regVal n (PP.pretty "ceilReal") e
                WEB.Cplx (realPart WUC.:+ imagPart) -> bindBin nonce regVal n (PP.pretty "complex") realPart imagPart
                WEB.RealPart e -> bindUnary nonce regVal n (PP.pretty "realPart") e
                WEB.ImagPart e -> bindUnary nonce regVal n (PP.pretty "imagPart") e

                WEB.StringContains e1 e2 -> bindBin nonce regVal n (PP.pretty "stringContains") e1 e2
                WEB.StringIsPrefixOf e1 e2 -> bindBin nonce regVal n (PP.pretty "stringIsPrefixOf") e1 e2
                WEB.StringIsSuffixOf e1 e2 -> bindBin nonce regVal n (PP.pretty "stringIsSuffixOf") e1 e2
                WEB.StringIndexOf e1 e2 e3 -> bindTernary nonce regVal n (PP.pretty "stringIndexOf") e1 e2 e3
                WEB.StringSubstring _srep e1 e2 e3 -> bindTernary nonce regVal n (PP.pretty "stringSubstring") e1 e2 e3
                WEB.StringLength e -> bindUnary nonce regVal n (PP.pretty "stringLength") e

                WEB.StructCtor reprs vs -> do
                  ops <- Ctx.forIndex (Ctx.size vs) (\macc idx -> do
                                                 acc <- macc
                                                 let e' = LMCR.RegEntry (LCT.baseToType (reprs Ctx.! idx)) (vs Ctx.! idx)
                                                 x <- traverseFormulaStructure n e'
                                                 return (x : acc)) (return [])
                  let doc = PP.hcat [ PP.pretty "structCtor"
                                    , ppArglist (reverse ops)
                                    ]
                  bind nonce regVal doc

bindUnary :: PN.Nonce s tp
          -> WEB.Expr s tp
          -> FormulaNode s (WEB.ExprBuilder s st fs)
          -> PP.Doc ()
          -> WEB.Expr s bt
          -> RegEntryBuilder s (WEB.ExprBuilder s st fs) Rendering
bindUnary nonce regVal n op e = do
  e' <- traverseFormulaStructure n (regEntry e)
  let doc = PP.hcat [ op, ppArglist [e'] ]
  bind nonce regVal doc

bindBin :: PN.Nonce s tp
        -> WEB.Expr s tp
        -> FormulaNode s (WEB.ExprBuilder s st fs)
        -> PP.Doc ()
        -> WEB.Expr s bt1
        -> WEB.Expr s bt2
        -> RegEntryBuilder s (WEB.ExprBuilder s st fs) Rendering
bindBin nonce regVal n op e1 e2 = do
  e1' <- traverseFormulaStructure n (regEntry e1)
  e2' <- traverseFormulaStructure n (regEntry e2)
  let doc = PP.hcat [ op, ppArglist [e1', e2'] ]
  bind nonce regVal doc

bindTernary :: PN.Nonce s tp
        -> WEB.Expr s tp
        -> FormulaNode s (WEB.ExprBuilder s st fs)
        -> PP.Doc ()
        -> WEB.Expr s bt1
        -> WEB.Expr s bt2
        -> WEB.Expr s bt3
        -> RegEntryBuilder s (WEB.ExprBuilder s st fs) Rendering
bindTernary nonce regVal n op e1 e2 e3 = do
  e1' <- traverseFormulaStructure n (regEntry e1)
  e2' <- traverseFormulaStructure n (regEntry e2)
  e3' <- traverseFormulaStructure n (regEntry e3)
  let doc = PP.hcat [ op, ppArglist [e1', e2', e3'] ]
  bind nonce regVal doc

bindBinaryFloat :: (Show a)
                => PN.Nonce s tp
                -> WEB.Expr s tp
                -> FormulaNode s (WEB.ExprBuilder s st fs)
                -> PP.Doc ()
                -> a
                -> WEB.Expr s bt1
                -> WEB.Expr s bt2
                -> RegEntryBuilder s (WEB.ExprBuilder s st fs) Rendering
bindBinaryFloat nonce regVal n op rm e1 e2 = do
  e1' <- traverseFormulaStructure n (regEntry e1)
  e2' <- traverseFormulaStructure n (regEntry e2)
  let doc = PP.hcat [ op, PP.tupled [PP.viaShow rm, ppRender e1', ppRender e2'] ]
  bind nonce regVal doc

regEntry :: (WI.IsExpr (WI.SymExpr sym))
         => WI.SymExpr sym bt
         -> LMCR.RegEntry sym (LCT.BaseToType bt)
regEntry e = LMCR.RegEntry (LCT.baseToType (WI.exprType e)) e

ppArglist :: [Rendering] -> PP.Doc ()
ppArglist = PP.tupled . fmap ppRender

ppRender :: Rendering -> PP.Doc ()
ppRender r =
  case r of
    Rendering doc -> doc
    RenderingInline doc -> doc

-- | Add a regular dependency edge from the src to the dest
addEdge :: FormulaNode s sym
        -> FormulaNode s sym
        -> RegEntryBuilder s sym ()
addEdge src dst = do
  MS.modify' $ \g -> g { builderEdges = Map.insertWith Set.union src (Set.singleton (dst, DependencyEdge)) (builderEdges g)
                       }

-- | Ensure that the node is in the graph (inserting an empty set of targets if it is not)
ensureNode :: FormulaNode s sym -> FormulaNodeLabel -> RegEntryBuilder s sym ()
ensureNode n nl = MS.modify' $ \g -> g { builderEdges = Map.insertWith Set.union n Set.empty (builderEdges g)
                                       , builderNodes = Map.insert n nl (builderNodes g)
                                       }

bind :: forall sym s st fs (tp :: LCT.BaseType)
      . (sym ~ WEB.ExprBuilder s st fs)
     => PN.Nonce s tp
     -> WEB.Expr s tp
     -> PP.Doc ()
     -> RegEntryBuilder s sym Rendering
bind nonce e doc = do
  MS.modify' $ \g -> g { builderEdges = Map.insertWith Set.union n Set.empty (builderEdges g)
                       , builderNodes = Map.insert n nl (builderNodes g)
                       , renderCache = Map.insert (CrucibleNonce nonce) ref (renderCache g)
                       }
  return ref
  where
    n = NonceNode nonce
    nl = NormalNodeLabel doc (Just (WEB.exprLoc e))
    ref = Rendering (PP.pretty "$" <> PP.pretty (PN.indexValue nonce))

regEntryToGraphViz :: (sym ~ WEB.ExprBuilder s st fs)
                   => LMCR.RegEntry sym tp
                   -> DG.DotGraph (FormulaNode s sym)
regEntryToGraphViz regEntry0 =
  DG.graphElemsToDot regEntryParams nodes edges
  where
    g = runRegEntryBuilder $ do
      let root = RootNode regEntry0
      rendering <- traverseFormulaStructure root regEntry0
      case rendering of
        RenderingInline doc -> ensureNode root (RootNodeLabel doc Nothing)
        Rendering doc -> ensureNode root (RootNodeLabel doc Nothing)

    nodes = Map.toList (builderNodes g)
    edges = [ (src, dst, elbl)
            | (src, labeledEdges) <- Map.toList (builderEdges g)
            , (dst, elbl) <- F.toList labeledEdges
            ]

-- | FIXME: Add clustering later
regEntryParams :: (sym ~ WEB.ExprBuilder s st fs)
               => DG.GraphvizParams (FormulaNode s sym) FormulaNodeLabel FormulaEdgeLabel () FormulaNodeLabel
regEntryParams =
  DG.Params { DG.isDirected = True
            , DG.globalAttributes = []
            , DG.clusterBy = DG.N
            , DG.isDotCluster = const False
            , DG.clusterID = \_ -> DG.Num (DG.Int 0)
            , DG.fmtCluster = \_ -> []
            , DG.fmtNode = formatRegEntryNode
            , DG.fmtEdge = formatRegEntryEdge
            }

formatRegEntryNode :: (sym ~ WEB.ExprBuilder s st fs) => (FormulaNode s sym, FormulaNodeLabel) -> DG.Attributes
formatRegEntryNode (_node, nodeLabel) =
  case nodeLabel of
    NormalNodeLabel doc Nothing -> [docLabel doc]
    RootNodeLabel doc Nothing -> [docLabel doc]

docLabel :: PP.Doc ann -> DG.Attribute
docLabel = DG.textLabel . PPT.renderLazy . PP.layoutCompact

formatRegEntryEdge :: (FormulaNode s sym, FormulaNode s sym, FormulaEdgeLabel) -> DG.Attributes
formatRegEntryEdge = undefined
