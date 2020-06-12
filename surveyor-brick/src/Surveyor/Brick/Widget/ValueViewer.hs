{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- | A viewer for Crucible run-time values (RegEntries)
module Surveyor.Brick.Widget.ValueViewer (
  ValueViewer,
  valueViewer,
  renderValueViewer
  ) where

import qualified Brick as B
import qualified Control.Monad.State.Strict as St
import qualified Data.List as L
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.Nonce as PN
import           Data.Proxy ( Proxy(..) )
import qualified Data.Text as T
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
                   , vsProxy :: Proxy sym
                   }

data ValueViewer s where
  ValueViewer :: ValueViewerState s sym tp -> ValueViewer s

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
  let vvs = ValueViewerState { regType = tp
                             , regValue = re
                             , cache = c
                             , rootWidget = renderedWidget root
                             , vsProxy = Proxy @sym
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
          return (RenderInline (B.txt "()"))
        CLM.LLVMPointerRepr _w ->
          case re of
            CLM.LLVMPointer base off -> do
              base' <- argRef <$> buildTermWidget (LCT.baseToType (WI.exprType base)) base
              off' <- argRef <$> buildTermWidget (LCT.baseToType (WI.exprType off)) off
              return (RenderInline (intersperse [B.txt "llvmPointer", base', off']))
        _ -> return (RenderInline (B.txt ("Unhandled crucible type " <> T.pack (show tp))))
    LCT.AsBaseType _btp ->
      case re of
        WEB.BoolExpr b _ -> return (RenderInline (B.txt (T.pack (show b))))
        WEB.SemiRingLiteral srep coeff _loc -> RenderInline <$> renderCoefficient srep coeff
        WEB.BoundVarExpr bv -> return (RenderInline (B.txt (WS.solverSymbolAsText (WEB.bvarName bv))))
        WEB.AppExpr ae -> do
          let nonce = WEB.appExprId ae
          m <- St.get
          case MapF.lookup nonce m of
            Just (ConstWidget cached) -> return cached
            Nothing ->
              case WEB.appExprApp ae of
                WEB.NotPred e -> bindUnaryExpr ae e "notPred"

                WEB.SemiRingSum ws -> do
                  let renderAdd e1 e2 = return $ intersperse [e1, B.txt "+", e2]
                  let scalarMult coef val = do
                        val' <- argRef <$> buildTermWidget (LCT.baseToType (WI.exprType val)) val
                        coefw <- renderCoefficient (WSum.sumRepr ws) coef
                        return (intersperse [coefw, B.txt "*", val'])
                  let constEval = renderCoefficient (WSum.sumRepr ws)
                  sumTerm <- WSum.evalM renderAdd scalarMult constEval ws
                  bindExpr ae sumTerm

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

                _ -> return (RenderInline (B.txt "Unhandled app"))

renderCoefficient :: (Monad m) => SR.SemiRingRepr sr -> SR.Coefficient sr -> m (B.Widget n)
renderCoefficient srep coeff =
  case srep of
    SR.SemiRingNatRepr -> return (B.str (show coeff))
    SR.SemiRingIntegerRepr -> return (B.str (show coeff))
    SR.SemiRingRealRepr -> return (B.str (show coeff))
    SR.SemiRingBVRepr bvFlv nr ->
      case bvFlv of
        SR.BVArithRepr -> return (B.str (show coeff))
        SR.BVBitsRepr -> return (B.str (printf "0x%x" coeff))

-- | Concatenate widgets with spaces in between
intersperse :: [B.Widget n] -> B.Widget n
intersperse = B.hBox . L.intersperse (B.txt " ")

bindBinExpr :: (sym ~ WEB.ExprBuilder s st fs)
            => WEB.AppExpr s tp1
            -> WI.SymExpr sym bt1
            -> WI.SymExpr sym bt2
            -> T.Text
            -> ViewerBuilder s sym RenderWidget
bindBinExpr ae e1 e2 name = do
  e1' <- argRef <$> buildTermWidget (LCT.baseToType (WI.exprType e1)) e1
  e2' <- argRef <$> buildTermWidget (LCT.baseToType (WI.exprType e2)) e2
  bindExpr ae (intersperse [B.txt name, e1', e2'])

bindUnaryExpr :: (sym ~ WEB.ExprBuilder s st fs)
              => WEB.AppExpr s tp1
              -> WI.SymExpr sym tp2
              -> T.Text
              -> ViewerBuilder s sym RenderWidget
bindUnaryExpr ae e name = do
  e' <- argRef <$> buildTermWidget (LCT.baseToType (WI.exprType e)) e
  bindExpr ae (intersperse [B.txt name, e'])

bindExpr :: WEB.AppExpr t tp -> B.Widget Names -> ViewerBuilder t sym RenderWidget
bindExpr ae w = returnCached (WEB.appExprId ae) $! RenderBound (thisRef ae) w

returnCached :: forall s sym (tp :: WT.BaseType)
              . PN.Nonce s tp
             -> RenderWidget
             -> ViewerBuilder s sym RenderWidget
returnCached nonce res = do
  St.modify' $ MapF.insert nonce (ConstWidget res)
  return res

thisRef :: WEB.AppExpr t tp -> B.Widget n
thisRef ae = B.txt (T.pack (show (WEB.appExprId ae)))

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

renderValueViewer :: ValueViewer s -> B.Widget Names
renderValueViewer (ValueViewer vs) =
  B.vBox ([ render w | MapF.Pair _ (ConstWidget w) <- MapF.toList (cache vs) ] ++ [rootWidget vs])
  where
    render rw =
      case rw of
        RenderInline _ -> error "Only bindings should exist in the map"
        RenderBound name w -> name B.<+> w
