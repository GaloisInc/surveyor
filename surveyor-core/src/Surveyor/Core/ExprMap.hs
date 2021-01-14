{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
-- | This module defines helpers for mapping from symbolic values, specifically
-- 'RegEntry', to arbitrary values.
--
-- This is a challenge because there is no Ord instance for 'RegEntry', nor
-- nonces.  Some subset of values have 'Ord' instances, so we decompose
-- composite values that do not into surrogate keys (in terms of 'SymExpr')
module Surveyor.Core.ExprMap (
  SymExpr(..),
  ExprMap,
  emptyExprMap,
  addExprValue,
  lookupExprValue,
  elems,
  ExprMapKey,
  baseValueKey,
  nonceValueKey,
  llvmPointerKey
  ) where

import qualified Data.List.NonEmpty as DLN
import qualified Data.Map.Strict as Map
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.Nonce as PN
import           Data.Parameterized.Some ( Some(..) )
import qualified Lang.Crucible.LLVM.MemModel as CLM
import qualified Lang.Crucible.Simulator.RegMap as LMCR
import qualified Lang.Crucible.Types as LCT
import qualified What4.BaseTypes as WT
import qualified What4.Expr.Builder as WEB

-- | A wrapper around values of type @a sym@ that provides a phantom base type parameter
newtype BaseTypeValue a (tp :: WT.BaseType) = BaseTypeValue { unwrapBaseTypeValue :: a }

-- This is a very simple wrapper around the 'WEB.Expr' type arranged so that we
-- can use them as surrogate keys in the cache.  The challenge is that we can't
-- expose the ExprBuilder in the type (without propagating it everywhere), but
-- we need it in order to get an Ord instance in scope.  Instead, we capture
-- that equality in an existential, which is enough to let us derive Eq/Ord.
data SymExpr s sym where
  SymExpr :: (sym ~ WEB.ExprBuilder s st fs) => Some (WEB.Expr s) -> SymExpr s sym

deriving instance Eq (SymExpr s sym)
deriving instance Ord (SymExpr s sym)

data ExprMapKey s sym (tp :: LCT.CrucibleType) where
  NonceKey :: !(PN.Nonce s bt) -> ExprMapKey s sym (LCT.BaseToType bt)
  SurrogateKey :: !(DLN.NonEmpty (SymExpr s sym)) -> ExprMapKey s sym tp

data ExprMap s sym a =
  ExprMap { nonceMap :: !(MapF.MapF (PN.Nonce s) (BaseTypeValue a))
          , surrogateMap :: !(Map.Map (DLN.NonEmpty (SymExpr s sym)) a)
          }

emptyExprMap :: ExprMap s sym a
emptyExprMap = ExprMap { nonceMap = MapF.empty, surrogateMap = mempty }

llvmPointerKey :: (sym ~ WEB.ExprBuilder s st fs)
               => LMCR.RegEntry sym (CLM.LLVMPointerType w)
               -> ExprMapKey s sym (CLM.LLVMPointerType w)
llvmPointerKey regEntry =
  case LMCR.regValue regEntry of
    CLM.LLVMPointer base off -> SurrogateKey ((SymExpr (Some base)) DLN.:| [SymExpr (Some off)])

baseValueKey :: (sym ~ WEB.ExprBuilder s st fs)
             => LMCR.RegEntry sym (LCT.BaseToType bt)
             -> Maybe (ExprMapKey s sym (LCT.BaseToType bt))
baseValueKey regEntry = NonceKey <$> WEB.exprMaybeId (LMCR.regValue regEntry)

nonceValueKey :: ( sym ~ WEB.ExprBuilder s st fs
                 , tp ~ LCT.BaseToType bt
                 )
              => PN.Nonce s bt
              -> ExprMapKey s sym tp
nonceValueKey nonce = NonceKey nonce

-- | Add a value to the 'ExprMap' at the given key
addExprValue :: ExprMapKey s sym tp
             -> a
             -> ExprMap s sym a
             -> ExprMap s sym a
addExprValue key val m =
  case key of
    NonceKey nonce -> m { nonceMap = MapF.insert nonce (BaseTypeValue val) (nonceMap m) }
    SurrogateKey sk -> m { surrogateMap = Map.insert sk val (surrogateMap m) }

lookupExprValue :: ExprMapKey s sym tp
                -> ExprMap s sym a
                -> Maybe a
lookupExprValue key m =
  case key of
    NonceKey nonce -> unwrapBaseTypeValue <$> MapF.lookup nonce (nonceMap m)
    SurrogateKey sk -> Map.lookup sk (surrogateMap m)

elems :: ExprMap s sym a -> [a]
elems (ExprMap nm skm) = Map.elems skm ++ [ v
                                          | Some (BaseTypeValue v) <- MapF.elems nm
                                          ]
