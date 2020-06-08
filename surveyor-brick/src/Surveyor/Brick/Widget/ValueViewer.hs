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
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.Nonce as PN
import           Data.Proxy ( Proxy(..) )
import qualified Data.Text as T
import qualified Lang.Crucible.Simulator.RegValue as LCSR
import qualified Lang.Crucible.Types as LCT
import qualified What4.BaseTypes as WT
import qualified What4.Expr.Builder as WEB

import           Surveyor.Brick.Names ( Names(..) )

data ValueViewerState s sym tp =
  ValueViewerState { regType :: LCT.TypeRepr tp
                   , regValue :: LCSR.RegValue sym tp
                   , cache :: MapF.MapF (PN.Nonce s) ConstWidget
                   -- ^ Previous translations of terms into widgets, cached to
                   -- avoid recomputation
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
  _ <- buildTermWidget tp re
  c <- St.get
  return (ValueViewer (ValueViewerState tp re c (Proxy @sym)))

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
    LCT.AsBaseType _btp ->
      case re of
        WEB.BoolExpr b _ -> return (RenderInline (B.txt (T.pack (show b))))
        WEB.AppExpr ae -> do
          let nonce = WEB.appExprId ae
          m <- St.get
          case MapF.lookup nonce m of
            Just (ConstWidget cached) -> return cached
            Nothing ->
              case WEB.appExprApp ae of
                WEB.NotPred e -> bindUnaryExpr ae LCT.BoolRepr e "notPred"

                WEB.RealIsInteger e -> bindUnaryExpr ae LCT.RealValRepr e "realIsInteger"

                WEB.NatDiv e1 e2 -> bindBinExpr ae LCT.NatRepr e1 e2 "natDiv"
                WEB.NatMod e1 e2 -> bindBinExpr ae LCT.NatRepr e1 e2 "natMod"

                WEB.IntDiv e1 e2 -> bindBinExpr ae LCT.IntegerRepr e1 e2 "intDiv"
                WEB.IntMod e1 e2 -> bindBinExpr ae LCT.IntegerRepr e1 e2 "intMod"

                WEB.RealDiv e1 e2 -> bindBinExpr ae LCT.RealValRepr e1 e2 "realDiv"
                WEB.RealSqrt e -> bindUnaryExpr ae LCT.RealValRepr e "realSqrt"

                WEB.RealSin e -> bindUnaryExpr ae LCT.RealValRepr e "realSin"
                WEB.RealCos e -> bindUnaryExpr ae LCT.RealValRepr e "realCos"
                WEB.RealATan2 e1 e2 -> bindBinExpr ae LCT.RealValRepr e1 e2 "realATan2"
                WEB.RealSinh e -> bindUnaryExpr ae LCT.RealValRepr e "realSinh"
                WEB.RealCosh e -> bindUnaryExpr ae LCT.RealValRepr e "realCosh"
                WEB.RealExp e -> bindUnaryExpr ae LCT.RealValRepr e "realExp"
                WEB.RealLog e -> bindUnaryExpr ae LCT.RealValRepr e "realLog"


bindBinExpr :: (sym ~ WEB.ExprBuilder s st fs)
            => WEB.AppExpr s tp1
            -> LCT.TypeRepr tp2
            -> LCSR.RegValue sym tp2
            -> LCSR.RegValue sym tp2
            -> T.Text
            -> ViewerBuilder s sym RenderWidget
bindBinExpr ae repr e1 e2 name = do
  e1' <- argRef <$> buildTermWidget repr e1
  e2' <- argRef <$> buildTermWidget repr e2
  bindExpr ae (B.txt name B.<+> e1' B.<+> e2')

bindUnaryExpr :: (sym ~ WEB.ExprBuilder s st fs)
              => WEB.AppExpr s tp1
              -> LCT.TypeRepr tp2
              -> LCSR.RegValue sym tp2
              -> T.Text
              -> ViewerBuilder s sym RenderWidget
bindUnaryExpr ae repr e name = do
  e' <- argRef <$> buildTermWidget repr e
  bindExpr ae (B.txt name B.<+> e')

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
renderValueViewer vv = B.vBox []
