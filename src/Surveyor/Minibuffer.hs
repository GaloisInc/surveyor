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
  Argument(..),
  Type(..),
  TypeRepr(..),
  IntType,
  WordType,
  AddressType,
  StringType,
  CommandType
  ) where

import qualified Data.Foldable as F
import qualified Data.Map as M
import           Data.Parameterized.Classes
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Text as T
import qualified Data.Text.Zipper.Generic as Z
import qualified Data.Vector as V
import           Numeric.Natural ( Natural )
import           Text.Read ( readMaybe )

import qualified Brick.Command as C
import qualified Brick.Match.Subword as SW
import qualified Brick.Widget.Minibuffer as MB
import qualified Surveyor.Architecture as A
import           Surveyor.Attributes

data Type where
  StringType :: Type
  AddressType :: Type
  IntType :: Type
  WordType :: Type
  CommandType :: Type

type StringType = 'StringType
type AddressType = 'AddressType
type IntType = 'IntType
type WordType = 'WordType
type CommandType = 'CommandType

data TypeRepr tp where
  CommandTypeRepr :: TypeRepr CommandType
  StringTypeRepr :: TypeRepr StringType
  AddressTypeRepr :: TypeRepr AddressType
  IntTypeRepr :: TypeRepr IntType
  WordTypeRepr :: TypeRepr WordType

instance TestEquality TypeRepr where
  testEquality CommandTypeRepr CommandTypeRepr = Just Refl
  testEquality StringTypeRepr StringTypeRepr = Just Refl
  testEquality AddressTypeRepr AddressTypeRepr = Just Refl
  testEquality IntTypeRepr IntTypeRepr = Just Refl
  testEquality WordTypeRepr WordTypeRepr = Just Refl
  testEquality _ _ = Nothing

data Argument arch s tp where
  CommandArgument :: Some (C.Command (Argument arch s) TypeRepr) -> Argument arch s CommandType
  StringArgument :: T.Text -> Argument arch s StringType
  AddressArgument :: A.Address arch s -> Argument arch s AddressType
  IntArgument :: Integer -> Argument arch s IntType
  WordArgument :: Natural -> Argument arch s WordType

parseArgument :: (A.Architecture arch s, Z.GenericTextZipper t)
              => [Some (C.Command (Argument arch s) TypeRepr)]
              -> t
              -> (TypeRepr tp -> Maybe (Argument arch s tp))
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

showRepr :: TypeRepr tp -> T.Text
showRepr r =
  case r of
    StringTypeRepr -> "String"
    AddressTypeRepr -> "Address"
    IntTypeRepr -> "Int"
    WordTypeRepr -> "Word"
    CommandTypeRepr -> "Command"

completeArgument :: (Z.GenericTextZipper t)
                 => [Some (C.Command (Argument arch s) TypeRepr)]
                 -> (t -> TypeRepr tp -> IO (V.Vector t))
completeArgument cmds =
  let cmdNames = V.fromList [ C.cmdName cmd | Some cmd <- cmds ]
  in \t r ->
    case r of
      StringTypeRepr -> return V.empty
      AddressTypeRepr -> return V.empty
      IntTypeRepr -> return V.empty
      WordTypeRepr -> return V.empty
      CommandTypeRepr ->
        case SW.matcher (Z.toList t) of
          Nothing -> return V.empty
          Just matcher -> do
            let matches = V.filter (SW.matches matcher) cmdNames
            let toGeneric txt = mconcat (map Z.singleton (T.unpack txt))
            return (fmap toGeneric matches)

minibuffer :: (Z.GenericTextZipper t, A.Architecture arch s)
           => n
           -- ^ The name of the editor widget
           -> n
           -- ^ The name of the completion list
           -> T.Text
           -> [Some (C.Command (Argument arch s) TypeRepr)]
           -> MB.Minibuffer (Argument arch s) TypeRepr t n
minibuffer edName compName pfx cmds =
  MB.minibuffer (parseArgument cmds) (completeArgument cmds) showRepr focusedListAttr edName compName pfx cmds
