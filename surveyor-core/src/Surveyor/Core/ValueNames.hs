{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Surveyor.Core.ValueNames (
  ValueNameMap,
  emptyValueNameMap,
  addValueName,
  lookupValueName,
  initialValueNames
  ) where

import           Control.Lens ( (^.) )
import qualified Control.Lens as L
import qualified Data.Foldable as F
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.Nonce as PN
import           Data.Parameterized.Some ( Some(..) )
import           Data.Proxy ( Proxy(..) )
import qualified Data.Text as T
import qualified Lang.Crucible.Backend as CB
import qualified Lang.Crucible.CFG.Core as LCCC
import qualified Lang.Crucible.FunctionHandle as CFH
import qualified Lang.Crucible.Simulator.CallFrame as LCSC
import qualified Lang.Crucible.Simulator.ExecutionTree as CSET
import qualified Lang.Crucible.Simulator.RegMap as LMCR
import qualified Lang.Crucible.Types as LCT
import qualified What4.BaseTypes as WT
import qualified What4.Expr.Builder as WEB
import qualified What4.FunctionName as WFN

import qualified Surveyor.Core.Architecture as CA

-- | A simple wrapper around user-specified names that has a monomorphic type parameter
--
-- The type parameter is not used, but without fixing it, we can get ambiguity
-- with the polymorphism of nonces.
data ValueName (tp :: WT.BaseType) where
  ValueName :: T.Text -> ValueName tp

valueNameText :: ValueName tp -> T.Text
valueNameText (ValueName t) = t

-- | A mapping from value identifiers (nonces) to names
--
-- This type is abstract to hide the annoyance of neeeding the 'ValueName'
-- wrapper from callers
newtype ValueNameMap s = ValueNameMap (MapF.MapF (PN.Nonce s) ValueName)

-- | Construct an empty 'ValueNameMap'
emptyValueNameMap :: ValueNameMap s
emptyValueNameMap = ValueNameMap MapF.empty

-- | Add a name for a value to the 'ValueNameMap'
--
-- Only values with 'PN.Nonce's can be named
addValueName :: PN.Nonce s (tp :: WT.BaseType) -> T.Text -> ValueNameMap s -> ValueNameMap s
addValueName nonce name (ValueNameMap m) =
  ValueNameMap (MapF.insert nonce (ValueName name) m)

lookupValueName :: PN.Nonce s (tp :: WT.BaseType) -> ValueNameMap s -> Maybe T.Text
lookupValueName nonce (ValueNameMap m) =
  valueNameText <$> MapF.lookup nonce m

-- | Construct an initial map of value names given a suspended simulator state
--
-- This walks back through all of the active call frames and adds synthetic
-- names where possible
initialValueNames :: forall arch s ext p sym rtp f a proxy st fs
                   . ( CA.SymbolicArchitecture arch s
                     , ext ~ CA.CrucibleExt arch
                     , sym ~ WEB.ExprBuilder s st fs
                     , CB.IsSymInterface sym
                     )
                  => proxy arch
                  -> CSET.SimState p sym ext rtp f a
                  -> ValueNameMap s
initialValueNames _ simState =
  F.foldl' addFrameNames emptyValueNameMap frames
  where
    frames = simState ^. CSET.stateTree . L.to CSET.activeFrames
    addFrameNames nms (CSET.SomeFrame sf) =
      case sf of
        LCSC.RF fn re ->
          let baseName = "ReturnFrom[" <> WFN.functionName fn <> "]"
          in F.foldl' (\nms' (CA.NamedTerm nonce nm) -> addValueName nonce nm nms') nms (nonceNames (Proxy @arch) baseName re)
        LCSC.OF (oframe :: LCSC.OverrideFrame sym ret args) ->
          let baseName = oframe ^. LCSC.override . L.to WFN.functionName
              regs = oframe ^. LCSC.overrideRegMap . L.to LMCR.regMap
              nonceWithBaseName :: forall (tp :: LCT.CrucibleType)
                                 . [[CA.NamedTerm s]]
                                -> Ctx.Index args tp
                                -> [[CA.NamedTerm s]]
              nonceWithBaseName acc idx =
                nonceNames (Proxy @arch) (baseName <> "@" <> T.pack (show idx)) (regs Ctx.! idx) : acc
              names = concat (Ctx.forIndex (Ctx.size regs) nonceWithBaseName [])
          in F.foldl' (\nms' (CA.NamedTerm nonce nm) -> addValueName nonce nm nms') nms names
        LCSC.MF (mframe@(LCSC.CallFrame { LCSC._frameCFG = cfg }) :: LCSC.CallFrame sym ext blocks ret args) ->
          let nArgs = if mframe ^. LCSC.frameBlockID == Some (LCCC.cfgEntryBlockID cfg)
                      then cfgArgCount cfg
                      else 0
              baseName = cfg ^. L.to LCCC.cfgHandle . L.to CFH.handleName . L.to WFN.functionName
              regs = mframe ^. LCSC.frameRegs . L.to LMCR.regMap
              nonceWithBaseName :: forall (tp :: LCT.CrucibleType)
                                 . [[CA.NamedTerm s]]
                                -> Ctx.Index args tp
                                -> [[CA.NamedTerm s]]
              nonceWithBaseName acc idx
                | Ctx.indexVal idx < nArgs =
                  nonceNames (Proxy @arch) (baseName <> "@" <> T.pack (show idx)) (regs Ctx.! idx) : acc
                | otherwise = acc
              names = concat (Ctx.forIndex (Ctx.size regs) nonceWithBaseName [])
          in F.foldl' (\nms' (CA.NamedTerm nonce nm) -> addValueName nonce nm nms') nms names

cfgArgCount :: LCCC.CFG ext blocks ctx ret -> Int
cfgArgCount cfg =
  Ctx.sizeInt (Ctx.size argReprs)
  where
    hdl = LCCC.cfgHandle cfg
    argReprs = CFH.handleArgTypes hdl

nonceNames :: forall arch s sym st fs tp proxy
            . ( sym ~ WEB.ExprBuilder s st fs
              , CA.SymbolicArchitecture arch s
              , CB.IsSymInterface sym
              )
           => proxy arch
           -> T.Text
           -> LMCR.RegEntry sym tp
           -> [CA.NamedTerm s]
nonceNames _ baseName re =
  case LCT.asBaseType (LMCR.regType re) of
    LCT.AsBaseType _btr ->
      case LMCR.regValue re of
        WEB.AppExpr ae -> [CA.NamedTerm (WEB.appExprId ae) baseName]
        WEB.NonceAppExpr nae -> [CA.NamedTerm (WEB.nonceExprId nae) baseName]
        WEB.BoundVarExpr bve -> [CA.NamedTerm (WEB.bvarId bve) baseName]
        WEB.SemiRingLiteral {} -> []
        WEB.BoolExpr {} -> []
        WEB.StringExpr {} -> []
    LCT.NotBaseType ->
      -- In cases where we don't have a base type, architecture-specific
      -- backends can provide their own naming scheme for intrinsic values
      case LMCR.regType re of
        LCT.IntrinsicRepr symRepr reprs -> CA.archNonceNames (Proxy @(arch, s)) baseName symRepr reprs re
        _ -> []
