{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ViewPatterns #-}
-- | The types of arguments for commands
module Surveyor.Arguments (
  Argument(..),
  Type(..),
  TypeRepr(..),
  IntType,
  WordType,
  AddressType,
  StringType,
  CommandType,
  FilePathType,
  showRepr,
  parseArgument
  ) where

import qualified Data.Foldable as F
import qualified Data.Map.Strict as M
import           Data.Parameterized.Classes
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Text as T
import qualified Data.Text.Zipper.Generic as Z
import           Numeric.Natural ( Natural )
import           Text.Read ( readMaybe )

import qualified Brick.Command as C
import qualified Surveyor.Architecture as A

data Type where
  StringType :: Type
  AddressType :: Type
  IntType :: Type
  WordType :: Type
  CommandType :: Type
  FilePathType :: Type

type StringType = 'StringType
type AddressType = 'AddressType
type IntType = 'IntType
type WordType = 'WordType
type CommandType = 'CommandType
type FilePathType = 'FilePathType

data TypeRepr tp where
  CommandTypeRepr :: TypeRepr CommandType
  StringTypeRepr :: TypeRepr StringType
  AddressTypeRepr :: TypeRepr AddressType
  IntTypeRepr :: TypeRepr IntType
  WordTypeRepr :: TypeRepr WordType
  FilePathTypeRepr :: TypeRepr FilePathType

instance TestEquality TypeRepr where
  testEquality CommandTypeRepr CommandTypeRepr = Just Refl
  testEquality StringTypeRepr StringTypeRepr = Just Refl
  testEquality AddressTypeRepr AddressTypeRepr = Just Refl
  testEquality IntTypeRepr IntTypeRepr = Just Refl
  testEquality WordTypeRepr WordTypeRepr = Just Refl
  testEquality FilePathTypeRepr FilePathTypeRepr = Just Refl
  testEquality _ _ = Nothing

data Argument arch e st s tp where
  CommandArgument :: Some (C.Command e st (Argument arch e st s) TypeRepr) -> Argument arch e st s CommandType
  StringArgument :: T.Text -> Argument arch e st s StringType
  AddressArgument :: A.Address arch s -> Argument arch e st s AddressType
  IntArgument :: Integer -> Argument arch e st s IntType
  WordArgument :: Natural -> Argument arch e st s WordType
  FilePathArgument :: FilePath -> Argument arch e st s FilePathType

parseArgument :: (A.Architecture arch s, Z.GenericTextZipper t)
              => [Some (C.Command e st (Argument arch e st s) TypeRepr)]
              -> t
              -> (TypeRepr tp -> Maybe (Argument arch e st s tp))
parseArgument cmds =
  let indexCommand m (Some cmd) = M.insert (C.cmdName cmd) (Some cmd) m
      cmdIndex = F.foldl' indexCommand M.empty cmds
  in \(Z.toList -> txt) rep ->
    case rep of
      StringTypeRepr -> Just (StringArgument (T.pack txt))
      IntTypeRepr -> IntArgument <$> readMaybe txt
      WordTypeRepr -> WordArgument <$> readMaybe txt
      AddressTypeRepr -> AddressArgument <$> A.parseAddress txt
      CommandTypeRepr ->
        let t = T.pack txt
        in CommandArgument <$> M.lookup t cmdIndex
      FilePathTypeRepr -> Just (FilePathArgument txt)

showRepr :: TypeRepr tp -> T.Text
showRepr r =
  case r of
    StringTypeRepr -> "String"
    AddressTypeRepr -> "Address"
    IntTypeRepr -> "Int"
    WordTypeRepr -> "Word"
    CommandTypeRepr -> "Command"
    FilePathTypeRepr -> "FilePath"
