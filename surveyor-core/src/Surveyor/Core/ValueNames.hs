{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module Surveyor.Core.ValueNames (
  ValueNameMap,
  emptyValueNameMap,
  addValueName,
  lookupValueName
  ) where

import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.Nonce as PN
import qualified Data.Text as T
import qualified What4.BaseTypes as WT

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

emptyValueNameMap :: ValueNameMap s
emptyValueNameMap = ValueNameMap MapF.empty

addValueName :: PN.Nonce s (tp :: WT.BaseType) -> T.Text -> ValueNameMap s -> ValueNameMap s
addValueName nonce name (ValueNameMap m) =
  ValueNameMap (MapF.insert nonce (ValueName name) m)

lookupValueName :: PN.Nonce s (tp :: WT.BaseType) -> ValueNameMap s -> Maybe T.Text
lookupValueName nonce (ValueNameMap m) =
  valueNameText <$> MapF.lookup nonce m
