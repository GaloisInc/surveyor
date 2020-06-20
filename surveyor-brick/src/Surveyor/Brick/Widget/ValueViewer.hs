{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- | A viewer for Crucible run-time values (RegEntries)
module Surveyor.Brick.Widget.ValueViewer (
  ValueViewer,
  valueViewer,
  renderValueViewer,
  handleValueViewerEvent
  ) where

import qualified Brick as B
import qualified Brick.Widgets.List as BL
import qualified Control.Monad.State.Strict as St
import qualified Data.BitVector.Sized as DBS
import qualified Data.List as L
import           Data.Maybe ( fromMaybe )
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.Nonce as PN
import qualified Data.Parameterized.TraversableFC as FC
import           Data.Proxy ( Proxy(..) )
import qualified Data.Text as T
import qualified Data.Vector as DV
import qualified Graphics.Vty as GV
import qualified Lang.Crucible.LLVM.MemModel as CLM
import qualified Lang.Crucible.Simulator.RegValue as LCSR
import qualified Lang.Crucible.Types as LCT
import           Text.Printf ( printf )
import qualified What4.BaseTypes as WT
import qualified What4.Expr.Builder as WEB
import qualified What4.Expr.WeightedSum as WSum
import qualified What4.Interface as WI
import qualified What4.SemiRing as SR
import qualified What4.Symbol as WS
import qualified What4.Utils.Complex as WUC
import qualified What4.Utils.StringLiteral as WUS

import           Surveyor.Brick.Names ( Names(..) )

data ValueViewerState s sym tp =
  ValueViewerState { regType :: LCT.TypeRepr tp
                   , regValue :: LCSR.RegValue sym tp
                   , cache :: MapF.MapF (PN.Nonce s) ConstWidget
                   -- ^ Previous translations of terms into widgets, cached to
                   -- avoid recomputation
                   , rootWidget :: B.Widget Names
                   -- ^ The top-level widget that demands the values defined in
                   -- the cache
                   , valueList :: BL.GenericList Names DV.Vector RenderWidget
                   -- ^ The state of the Brick list we use to render values
                   --
                   -- This tracks things like selection state
                   , vsProxy :: Proxy sym
                   }

-- | A widget state backing the value viewer; it existentially quantifies away
-- the type and symbolic backend, with the real data storage in
-- 'ValueViewerState'
data ValueViewer s where
  ValueViewer :: ValueViewerState s sym tp -> ValueViewer s

-- | Construct a 'ValueViewer' for a single value
--
-- Note that 'LCSR.RegValue' is a type family, and we need the 'LCT.TypeRepr' to
-- know how to interpret the value (i.e., determine its representation).  We can
-- only touch values under a case expression over the type repr.
--
-- Also note that we have concretized the symbolic backend type, as we need to
-- know what the underlying representation is to case over all of the
-- constructors.
--
-- Note that we eagerly construct most of the state for the viewer, but we do it
-- in a way that preserves sharing to avoid re-traversing terms (see
-- 'buildViewer')
valueViewer :: forall proxy sym tp s st fs
             . (sym ~ WEB.ExprBuilder s st fs)
            => proxy sym
            -> LCT.TypeRepr tp
            -> LCSR.RegValue sym tp
            -> ValueViewer s
valueViewer p tp re =
  St.evalState (unViewBuilder (buildViewer p tp re)) MapF.empty

-- | Mark widgets as either being rendered inline in operand positions or as
-- generating a binding that will be referred to by a number in an operand
-- position
data RenderWidget = RenderInline (B.Widget Names)
                  | RenderBound (B.Widget Names) (B.Widget Names)

renderedWidget :: RenderWidget -> B.Widget Names
renderedWidget rw =
  case rw of
    RenderInline w -> w
    RenderBound _name w -> w

-- | This is a simple wrapper around a 'RenderWidget' to throw away a type
-- parameter so that we can store it in a 'MapF.MapF'.  We need a custom type
-- instead of just using 'Data.Functor.Const' because that type is too
-- polymorphic and causes some type inference problems.
newtype ConstWidget (tp :: WT.BaseType) = ConstWidget RenderWidget

newtype ViewerBuilder s sym a =
  ViewerBuilder { unViewBuilder :: St.State (MapF.MapF (PN.Nonce s) ConstWidget) a
                }
  deriving ( Functor
           , Monad
           , Applicative
           , St.MonadState (MapF.MapF (PN.Nonce s) ConstWidget)
           )

-- | Build a 'ValueViewer' by traversing the value and building brick widgets as
-- we encounter terms.  It caches previously-seen terms by their nonce value.
-- Values without nonces are always rendered inline.
buildViewer :: forall proxy sym tp s st fs
             . (sym ~ WEB.ExprBuilder s st fs)
            => proxy sym
            -> LCT.TypeRepr tp
            -> LCSR.RegValue sym tp
            -> ViewerBuilder s sym (ValueViewer s)
buildViewer _p tp re = do
  -- Traverse the whole term initially to cache any widgets we can based on nonces.
  --
  -- We'll use this same primitive for rendering later, but then we'll start it
  -- with a primed cache
  root <- buildTermWidget tp re
  c <- St.get
  let vals = DV.fromList ([ w | MapF.Pair _ (ConstWidget w) <- MapF.toList c ] ++ [root])
  let vvs = ValueViewerState { regType = tp
                             , regValue = re
                             , cache = c
                             , rootWidget = renderedWidget root
                             , vsProxy = Proxy @sym
                             , valueList = BL.list ValueViewerList vals 1
                             }
  return (ValueViewer vvs)

buildTermWidget :: (sym ~ WEB.ExprBuilder s st fs)
                => LCT.TypeRepr tp
                -> LCSR.RegValue sym tp
                -> ViewerBuilder s sym RenderWidget
buildTermWidget tp re =
  case LCT.asBaseType tp of
    LCT.NotBaseType ->
      case tp of
        LCT.UnitRepr ->
          -- We don't update the cache here because we don't have a nonce for these
          inline (B.txt "()")
        CLM.LLVMPointerRepr _w ->
          case re of
            CLM.LLVMPointer base off -> do
              base' <- argRef <$> buildTermWidget (LCT.baseToType (WI.exprType base)) base
              off' <- argRef <$> buildTermWidget (LCT.baseToType (WI.exprType off)) off
              inline (intersperse [B.txt "llvmPointer", base', off'])
        _ -> inline (B.txt ("Unhandled crucible type " <> T.pack (show tp)))
    LCT.AsBaseType _btp ->
      case re of
        WEB.BoolExpr b _ -> inline (B.txt (T.pack (show b)))
        WEB.SemiRingLiteral srep coeff _loc -> RenderInline <$> renderCoefficient srep coeff
        WEB.StringExpr sl _loc ->
          case sl of
            WUS.UnicodeLiteral t -> inline (B.txt "\"" B.<+> B.txt t B.<+> B.txt "\"")
            WUS.Char8Literal bs -> inline (B.str (show bs))
            WUS.Char16Literal ws -> inline (B.str (show ws))
        WEB.BoundVarExpr bv ->
          inline (B.txt "$" B.<+> B.txt (WS.solverSymbolAsText (WEB.bvarName bv)))
        WEB.NonceAppExpr nae ->
          case WEB.nonceExprApp nae of
            WEB.Annotation bt annotNonce e -> do
              e' <- argRef <$> buildTermWidget (LCT.baseToType bt) e
              bindExpr nae (intersperse [ B.txt "annotated"
                                        , B.str (printf "[$%d]" (PN.indexValue annotNonce))
                                        , e'
                                        ])
            WEB.Forall bv e -> do
              e' <- argRef <$> buildTermWidget (LCT.baseToType (WI.exprType e)) e
              bindExpr nae (intersperse [ B.txt "forall"
                                        , B.txt "$" B.<+> B.txt (WS.solverSymbolAsText (WEB.bvarName bv))
                                        , B.txt "."
                                        , e'
                                        ])
            WEB.Exists bv e -> do
              e' <- argRef <$> buildTermWidget (LCT.baseToType (WI.exprType e)) e
              bindExpr nae (intersperse [ B.txt "exists"
                                        , B.txt "$" B.<+> B.txt (WS.solverSymbolAsText (WEB.bvarName bv))
                                        , B.txt "."
                                        , e'
                                        ])
            WEB.ArrayFromFn sf -> do
              bindExpr nae (intersperse [ B.txt "arrayFromFn"
                                        , B.txt (WS.solverSymbolAsText (WEB.symFnName sf))
                                        ])
            WEB.FnApp sf args -> do
              let es = FC.toListFC (\e -> buildTermWidget (LCT.baseToType (WI.exprType e)) e) args
              es' <- fmap argRef <$> sequence es
              bindExpr nae (intersperse ( B.txt (WS.solverSymbolAsText (WEB.symFnName sf))
                                        : es'
                                        ))
            WEB.MapOverArrays sf _reprs vals -> do
              let es = FC.toListFC (\(WI.ArrayResultWrapper e) -> buildTermWidget (LCT.baseToType (WI.exprType e)) e) vals
              es' <- fmap argRef <$> sequence es
              bindExpr nae (intersperse ( B.txt "mapOverArrays"
                                        : B.txt (WS.solverSymbolAsText (WEB.symFnName sf))
                                        : es'
                                        ))
            WEB.ArrayTrueOnEntries sf e -> do
              e' <- argRef <$> buildTermWidget (LCT.baseToType (WI.exprType e)) e
              bindExpr nae (intersperse [ B.txt "arrayTrueOnEntries"
                                        , B.txt (WS.solverSymbolAsText (WEB.symFnName sf))
                                        , e'
                                        ])
        WEB.AppExpr ae -> do
          let nonce = WEB.appExprId ae
          m <- St.get
          case MapF.lookup nonce m of
            Just (ConstWidget cached) -> return cached
            Nothing ->
              case WEB.appExprApp ae of
                WEB.BaseEq _tp e1 e2 -> bindBinExpr ae e1 e2 "eq"
                WEB.BaseIte _tp _nPred e1 e2 e3 -> bindTernaryExpr ae e1 e2 e3 "ite"
                WEB.NotPred e -> bindUnaryExpr ae e "not"

                WEB.SemiRingSum ws -> do
                  let (addOp, mulOp) = case WSum.sumRepr ws of
                        SR.SemiRingRealRepr -> ("realSum", "realMul")
                        SR.SemiRingIntegerRepr -> ("intSum", "intMul")
                        SR.SemiRingNatRepr -> ("natSum", "natMul")
                        SR.SemiRingBVRepr SR.BVArithRepr _w -> ("bvSum", "bvMul")
                        SR.SemiRingBVRepr SR.BVBitsRepr _w -> ("bvXor", "bvAnd")
                  let renderAdd e1 e2 = return (B.txt "(" B.<+> intersperse [B.txt addOp, e1, e2] B.<+> B.txt ")")
                  let scalarMult coef val = do
                        val' <- argRef <$> buildTermWidget (LCT.baseToType (WI.exprType val)) val
                        coefw <- renderCoefficient (WSum.sumRepr ws) coef
                        return (B.txt "(" B.<+> intersperse [B.txt mulOp, coefw, val'] B.<+> B.txt ")")
                  let constEval = renderCoefficient (WSum.sumRepr ws)
                  sumTerm <- WSum.evalM renderAdd scalarMult constEval ws
                  bindExpr ae sumTerm

                WEB.SemiRingProd rp -> do
                  let op = case WSum.prodRepr rp of
                        SR.SemiRingRealRepr -> "realProd"
                        SR.SemiRingIntegerRepr -> "intProd"
                        SR.SemiRingNatRepr -> "natProd"
                        SR.SemiRingBVRepr SR.BVArithRepr _w -> "bvProd"
                        SR.SemiRingBVRepr SR.BVBitsRepr _w -> "bvAnd"
                  let mul acc e = return (e ++ acc)
                  let tm val = do
                        rw <- buildTermWidget (LCT.baseToType (WI.exprType val)) val
                        return [argRef rw]
                  mProdTerms <- WSum.prodEvalM mul tm rp
                  let prodTerms = fromMaybe [] mProdTerms
                  bindExpr ae (intersperse (B.txt op : prodTerms))

                WEB.RealIsInteger e -> bindUnaryExpr ae e "realIsInteger"

                WEB.NatDiv e1 e2 -> bindBinExpr ae e1 e2 "natDiv"
                WEB.NatMod e1 e2 -> bindBinExpr ae e1 e2 "natMod"

                WEB.IntDiv e1 e2 -> bindBinExpr ae e1 e2 "intDiv"
                WEB.IntMod e1 e2 -> bindBinExpr ae e1 e2 "intMod"

                WEB.RealDiv e1 e2 -> bindBinExpr ae e1 e2 "realDiv"
                WEB.RealSqrt e -> bindUnaryExpr ae e "realSqrt"

                WEB.RealSin e -> bindUnaryExpr ae e "realSin"
                WEB.RealCos e -> bindUnaryExpr ae e "realCos"
                WEB.RealATan2 e1 e2 -> bindBinExpr ae e1 e2 "realATan2"
                WEB.RealSinh e -> bindUnaryExpr ae e "realSinh"
                WEB.RealCosh e -> bindUnaryExpr ae e "realCosh"
                WEB.RealExp e -> bindUnaryExpr ae e "realExp"
                WEB.RealLog e -> bindUnaryExpr ae e "realLog"

                WEB.BVTestBit bitNum e -> do
                  e' <- argRef <$> buildTermWidget (LCT.baseToType (WI.exprType e)) e
                  bindExpr ae (intersperse [B.txt "bvTestBit", B.str (show bitNum), e'])

                WEB.BVSlt e1 e2 -> bindBinExpr ae e1 e2 "bvSlt"
                WEB.BVUlt e1 e2 -> bindBinExpr ae e1 e2 "bvUlt"

                WEB.BVUdiv _rep e1 e2 -> bindBinExpr ae e1 e2 "bvUdiv"
                WEB.BVUrem _rep e1 e2 -> bindBinExpr ae e1 e2 "bvUrem"
                WEB.BVSdiv _rep e1 e2 -> bindBinExpr ae e1 e2 "bvSdiv"
                WEB.BVSrem _rep e1 e2 -> bindBinExpr ae e1 e2 "bvSrem"
                WEB.BVShl _rep e1 e2 -> bindBinExpr ae e1 e2 "bvShl"
                WEB.BVLshr _rep e1 e2 -> bindBinExpr ae e1 e2 "bvLshr"
                WEB.BVAshr _rep e1 e2 -> bindBinExpr ae e1 e2 "bvAshr"
                WEB.BVRol _rep e1 e2 -> bindBinExpr ae e1 e2 "bvRol"
                WEB.BVRor _rep e1 e2 -> bindBinExpr ae e1 e2 "bvRor"
                WEB.BVZext _rep e -> bindUnaryExpr ae e "bvZext"
                WEB.BVSext _rep e -> bindUnaryExpr ae e "bvSext"
                WEB.BVPopcount _rep e -> bindUnaryExpr ae e "bvPopcount"
                WEB.BVCountTrailingZeros _rep e -> bindUnaryExpr ae e "bvCtz"
                WEB.BVCountLeadingZeros _rep e -> bindUnaryExpr ae e "bvClz"

                WEB.FloatPZero _rep -> inline (B.txt "+0")
                WEB.FloatNZero _rep -> inline (B.txt "-0")
                WEB.FloatNaN _rep -> inline (B.txt "NaN")
                WEB.FloatPInf _rep -> inline (B.txt "+Inf")
                WEB.FloatNInf _rep -> inline (B.txt "-Inf")
                WEB.FloatNeg _rep e -> bindUnaryExpr ae e "floatNeg"
                WEB.FloatAbs _rep e -> bindUnaryExpr ae e "floatAbs"
                WEB.FloatSqrt _rep rm e -> bindUnaryFloatExpr ae rm e "floatSqrt"
                WEB.FloatAdd _rep rm e1 e2 -> bindBinaryFloatExpr ae rm e1 e2 "floatAdd"
                WEB.FloatSub _rep rm e1 e2 -> bindBinaryFloatExpr ae rm e1 e2 "floatSub"
                WEB.FloatMul _rep rm e1 e2 -> bindBinaryFloatExpr ae rm e1 e2 "floatMul"
                WEB.FloatDiv _rep rm e1 e2 -> bindBinaryFloatExpr ae rm e1 e2 "floatDiv"
                WEB.FloatRem _rep e1 e2 -> bindBinExpr ae e1 e2 "floatRem"
                WEB.FloatMin _rep e1 e2 -> bindBinExpr ae e1 e2 "floatMin"
                WEB.FloatMax _rep e1 e2 -> bindBinExpr ae e1 e2 "floatMax"
                WEB.FloatFpEq e1 e2 -> bindBinExpr ae e1 e2 "floatFpEq"
                WEB.FloatFpNe e1 e2 -> bindBinExpr ae e1 e2 "floatFpNe"
                WEB.FloatLe e1 e2 -> bindBinExpr ae e1 e2 "floatLe"
                WEB.FloatLt e1 e2 -> bindBinExpr ae e1 e2 "floatLt"
                WEB.FloatIsNaN e -> bindUnaryExpr ae e "floatIsNaN"
                WEB.FloatIsInf e -> bindUnaryExpr ae e "floatIsInf"
                WEB.FloatIsZero e -> bindUnaryExpr ae e "floatIsZero"
                WEB.FloatIsPos e -> bindUnaryExpr ae e "floatIsPos"
                WEB.FloatIsNeg e -> bindUnaryExpr ae e "floatIsNeg"
                WEB.FloatIsSubnorm e -> bindUnaryExpr ae e "floatIsSubnorm"
                WEB.FloatIsNorm e -> bindUnaryExpr ae e "floatIsNorm"

                WEB.NatToInteger e -> bindUnaryExpr ae e "natToInteger"
                WEB.IntegerToNat e -> bindUnaryExpr ae e "integerToNat"
                WEB.IntegerToReal e -> bindUnaryExpr ae e "integerToReal"
                WEB.RealToInteger e -> bindUnaryExpr ae e "realToInteger"
                WEB.BVToNat e -> bindUnaryExpr ae e "bvToNat"
                WEB.BVToInteger e -> bindUnaryExpr ae e "bvToInteger"
                WEB.SBVToInteger e -> bindUnaryExpr ae e "sbvToInteger"


                WEB.RoundReal e -> bindUnaryExpr ae e "roundReal"
                WEB.FloorReal e -> bindUnaryExpr ae e "floorReal"
                WEB.CeilReal e -> bindUnaryExpr ae e "ceilReal"
                WEB.Cplx (realPart WUC.:+ imagPart) -> bindBinExpr ae realPart imagPart "complex"
                WEB.RealPart e -> bindUnaryExpr ae e "realPart"
                WEB.ImagPart e -> bindUnaryExpr ae e "imagPart"

                WEB.StringContains e1 e2 -> bindBinExpr ae e1 e2 "stringContains"
                WEB.StringIsPrefixOf e1 e2 -> bindBinExpr ae e1 e2 "stringIsPrefixOf"
                WEB.StringIsSuffixOf e1 e2 -> bindBinExpr ae e1 e2 "stringIsSuffixOf"
                WEB.StringIndexOf e1 e2 e3 -> bindTernaryExpr ae e1 e2 e3 "stringIndexOf"
                WEB.StringSubstring _srep e1 e2 e3 -> bindTernaryExpr ae e1 e2 e3 "stringSubstring"
                WEB.StringLength e -> bindUnaryExpr ae e "stringLength"

                WEB.StructCtor _reprs vs -> do
                  let vs' = FC.toListFC (\e -> buildTermWidget (LCT.baseToType (WI.exprType e)) e) vs
                  vs'' <- fmap argRef <$> sequence vs'
                  bindExpr ae (intersperse ( B.txt "structCtor"
                                           : vs''
                                           ))


                _ -> inline (B.txt "Unhandled app")

inline :: (Monad m) => B.Widget Names -> m RenderWidget
inline = return . RenderInline



renderCoefficient :: (Monad m) => SR.SemiRingRepr sr -> SR.Coefficient sr -> m (B.Widget n)
renderCoefficient srep coeff =
  case srep of
    SR.SemiRingNatRepr -> return (B.str (show coeff))
    SR.SemiRingIntegerRepr -> return (B.str (show coeff))
    SR.SemiRingRealRepr -> return (B.str (show coeff))
    SR.SemiRingBVRepr bvFlv nr ->
      case bvFlv of
        SR.BVArithRepr -> return (B.str (show coeff))
        SR.BVBitsRepr -> return (B.str (DBS.ppHex nr coeff))

renderRoundingMode :: WI.RoundingMode -> B.Widget n
renderRoundingMode rm =
  case rm of
    WI.RNE -> B.txt "rne"
    WI.RNA -> B.txt "rna"
    WI.RTP -> B.txt "rtp"
    WI.RTN -> B.txt "rtn"
    WI.RTZ -> B.txt "rtz"

bindUnaryFloatExpr :: (sym ~ WEB.ExprBuilder s st fs)
                   => WEB.AppExpr s tp1
                   -> WI.RoundingMode
                   -> WI.SymExpr sym bt1
                   -> T.Text
                   -> ViewerBuilder s sym RenderWidget
bindUnaryFloatExpr ae rm e name = do
  e' <- argRef <$> buildTermWidget (LCT.baseToType (WI.exprType e)) e
  let rmw = renderRoundingMode rm
  bindExpr ae (intersperse [B.txt name, rmw, e'])

bindBinaryFloatExpr :: (sym ~ WEB.ExprBuilder s st fs)
                    => WEB.AppExpr s tp1
                    -> WI.RoundingMode
                    -> WI.SymExpr sym bt1
                    -> WI.SymExpr sym bt2
                    -> T.Text
                    -> ViewerBuilder s sym RenderWidget
bindBinaryFloatExpr ae rm e1 e2 name = do
  e1' <- argRef <$> buildTermWidget (LCT.baseToType (WI.exprType e1)) e1
  e2' <- argRef <$> buildTermWidget (LCT.baseToType (WI.exprType e2)) e2
  let rmw = renderRoundingMode rm
  bindExpr ae (intersperse [B.txt name, rmw, e1', e2'])

-- | Concatenate widgets with spaces in between
intersperse :: [B.Widget n] -> B.Widget n
intersperse = B.hBox . L.intersperse (B.txt " ")

class HasNonce e where
  getNonce :: e s (tp :: WI.BaseType) -> PN.Nonce s tp

instance HasNonce WEB.AppExpr where
  getNonce = WEB.appExprId

instance HasNonce WEB.NonceAppExpr where
  getNonce = WEB.nonceExprId

bindTernaryExpr :: ( sym ~ WEB.ExprBuilder s st fs
                   , HasNonce e
                   )
                => e s tp1
                -> WI.SymExpr sym bt1
                -> WI.SymExpr sym bt2
                -> WI.SymExpr sym bt3
                -> T.Text
                -> ViewerBuilder s sym RenderWidget
bindTernaryExpr ae e1 e2 e3 name = do
  e1' <- argRef <$> buildTermWidget (LCT.baseToType (WI.exprType e1)) e1
  e2' <- argRef <$> buildTermWidget (LCT.baseToType (WI.exprType e2)) e2
  e3' <- argRef <$> buildTermWidget (LCT.baseToType (WI.exprType e3)) e3
  bindExpr ae (intersperse [B.txt name, e1', e2', e3'])

bindBinExpr :: ( sym ~ WEB.ExprBuilder s st fs
               , HasNonce e
               )
            => e s tp1
            -> WI.SymExpr sym bt1
            -> WI.SymExpr sym bt2
            -> T.Text
            -> ViewerBuilder s sym RenderWidget
bindBinExpr ae e1 e2 name = do
  e1' <- argRef <$> buildTermWidget (LCT.baseToType (WI.exprType e1)) e1
  e2' <- argRef <$> buildTermWidget (LCT.baseToType (WI.exprType e2)) e2
  bindExpr ae (intersperse [B.txt name, e1', e2'])

bindUnaryExpr :: ( sym ~ WEB.ExprBuilder s st fs
                 , HasNonce e
                 )
              => e s tp1
              -> WI.SymExpr sym tp2
              -> T.Text
              -> ViewerBuilder s sym RenderWidget
bindUnaryExpr ae e name = do
  e' <- argRef <$> buildTermWidget (LCT.baseToType (WI.exprType e)) e
  bindExpr ae (intersperse [B.txt name, e'])

bindExpr :: (HasNonce e) => e t tp -> B.Widget Names -> ViewerBuilder t sym RenderWidget
bindExpr ae w = returnCached (getNonce ae) $! RenderBound (thisRef ae) w

returnCached :: forall s sym (tp :: WT.BaseType)
              . PN.Nonce s tp
             -> RenderWidget
             -> ViewerBuilder s sym RenderWidget
returnCached nonce res = do
  St.modify' $ MapF.insert nonce (ConstWidget res)
  return res

thisRef :: (HasNonce e) => e t tp -> B.Widget n
thisRef ae = B.str (printf "$%d" (PN.indexValue (getNonce ae)))

-- | Extract a widget that should be used to refer to the given 'RenderWidget'
-- as an operand
--
-- For values that should be rendered inline, it is the value itself
--
-- For compound values that must be bound, this widget is the unique identifier
-- bound to the term
argRef :: RenderWidget -> B.Widget Names
argRef widget =
  case widget of
    RenderInline w -> w
    RenderBound r _ -> r

renderValueViewer :: Bool -> ValueViewer s -> B.Widget Names
renderValueViewer viewerFocused (ValueViewer vs) =
  BL.renderList render viewerFocused (valueList vs)
  where
    render _hasFocus rw =
      case rw of
        RenderInline w -> w
        RenderBound name w -> intersperse [name, B.txt "=", w]

handleValueViewerEvent :: GV.Event
                       -> ValueViewer s
                       -> B.EventM Names (ValueViewer s)
handleValueViewerEvent evt (ValueViewer vs) = do
  l' <- BL.handleListEvent evt (valueList vs)
  return (ValueViewer vs { valueList = l' })
