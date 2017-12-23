{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ViewPatterns #-}
-- | An instantiation of the generic minibuffer widget with a specific argument type
module Surveyor.Minibuffer (
  MB.Minibuffer,
  minibuffer,
  MB.MinibufferStatus(..),
  MB.handleMinibufferEvent,
  MB.renderMinibuffer,
  MB.Command(..),
  Argument(..),
  Type(..),
  TypeRepr(..),
  IntType,
  WordType,
  AddressType,
  StringType
  ) where

import           Data.Parameterized.Classes
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Text as T
import qualified Data.Text.Zipper.Generic as Z
import           Data.Word ( Word64 )
import           Numeric.Natural ( Natural )
import           Text.Read ( readMaybe )

import           Surveyor.Attributes
import qualified Surveyor.Widget.Minibuffer as MB

data Type where
  StringType :: Type
  AddressType :: Type
  IntType :: Type
  WordType :: Type

type StringType = 'StringType
type AddressType = 'AddressType
type IntType = 'IntType
type WordType = 'WordType

data TypeRepr tp where
  StringTypeRepr :: TypeRepr StringType
  AddressTypeRepr :: TypeRepr AddressType
  IntTypeRepr :: TypeRepr IntType
  WordTypeRepr :: TypeRepr WordType

instance TestEquality TypeRepr where
  testEquality StringTypeRepr StringTypeRepr = Just Refl
  testEquality AddressTypeRepr AddressTypeRepr = Just Refl
  testEquality IntTypeRepr IntTypeRepr = Just Refl
  testEquality WordTypeRepr WordTypeRepr = Just Refl
  testEquality _ _ = Nothing

data Argument tp where
  StringArgument :: T.Text -> Argument StringType
  AddressArgument :: Word64 -> Argument AddressType
  IntArgument :: Integer -> Argument IntType
  WordArgument :: Natural -> Argument WordType

parseArgument :: (Z.GenericTextZipper t) => t -> TypeRepr tp -> Maybe (Argument tp)
parseArgument (Z.toList -> txt) rep =
  case rep of
    StringTypeRepr -> Just (StringArgument (T.pack txt))
    IntTypeRepr -> IntArgument <$> readMaybe txt
    WordTypeRepr -> WordArgument <$> readMaybe txt
    AddressTypeRepr -> AddressArgument <$> readMaybe txt

showRepr :: TypeRepr tp -> T.Text
showRepr r =
  case r of
    StringTypeRepr -> "String"
    AddressTypeRepr -> "Address"
    IntTypeRepr -> "Int"
    WordTypeRepr -> "Word"

minibuffer :: (Z.GenericTextZipper t)
           => n
           -- ^ The name of the editor widget
           -> n
           -- ^ The name of the completion list
           -> T.Text
           -> [Some (MB.Command Argument TypeRepr)]
           -> MB.Minibuffer Argument TypeRepr t n
minibuffer = MB.minibuffer parseArgument showRepr focusedListAttr
