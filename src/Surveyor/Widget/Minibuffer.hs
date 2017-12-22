{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- | An emacs-style minibuffer as a Brick widget
module Surveyor.Widget.Minibuffer (
  Minibuffer,
  minibuffer,
  handleMinibufferEvent,
  renderMinibuffer,
  Type(..),
  TypeRepr(..),
  Argument(..),
  Command(..),
  IntType,
  WordType,
  AddressType,
  StringType
  ) where

import qualified Brick as B
import qualified Brick.Widgets.Edit as B
import           Control.Monad.IO.Class ( liftIO )
import qualified Data.Foldable as F
import qualified Data.Functor.Const as C
import qualified Data.List as L
import qualified Data.Map as M
import           Data.Parameterized.Classes
import qualified Data.Parameterized.List as PL
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.Zipper.Generic as Z
import           Data.Word ( Word64 )
import qualified Graphics.Vty as V
import           Numeric.Natural ( Natural )
import qualified Text.RE.TDFA.Text as RE

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

data Command where
  Command :: T.Text
          -- ^ Command name
          -> PL.List (C.Const T.Text) tps
          -- ^ Argument names
          -> PL.List TypeRepr tps
          -- ^ Argument types
          -> (PL.List Argument tps -> IO ())
          -- ^ Function to call on the argument list
          -> Command

data MinibufferState where
  CollectingArguments :: PL.List (C.Const T.Text) tps
                      -> PL.List TypeRepr tps
                      -> PL.List TypeRepr tps'
                      -> PL.List Argument tps'
                      -> PL.List TypeRepr tps0
                      -> (PL.List Argument tps0 -> IO ())
                      -> MinibufferState
  -- ^ In the process of collecting arguments
  Editing :: MinibufferState
  -- ^ The input editor has focus

-- | The abstract state of a minibuffer
--
-- The @t@ parameter is the type of the content (probably 'T.Text', but anything
-- that implements 'GenericTextZipper').
--
-- The @n@ parameter is the type of the names used to identify widgets in your
-- application.
data Minibuffer t n =
  Minibuffer { prefix :: !T.Text
             , editor :: !(B.Editor t n)
             , allCommands :: !(Seq.Seq Command)
             , commandIndex :: !(M.Map T.Text Command)
             , matchedCommands :: !(Seq.Seq Command)
             , selectedMatch :: Maybe Int
             , state :: MinibufferState
             }

-- | Create a new 'Minibuffer' state.
--
-- The minibuffer supports the given set of commands
minibuffer :: (Z.GenericTextZipper t)
           => n
           -- ^ The name to assign to the editor widget
           -> T.Text
           -- ^ The prefix to display before the editor widget
           -> [Command]
           -- ^ The commands supported by the minibuffer
           -> Minibuffer t n
minibuffer edName pfx cmds =
  Minibuffer { prefix = pfx
             , editor = B.editor edName (Just 1) mempty
             , allCommands = Seq.fromList cmds
             , commandIndex = F.foldl' indexCommand M.empty cmds
             , matchedCommands = Seq.empty
             , selectedMatch = Nothing
             , state = Editing
             }
  where
    indexCommand m cmd@(Command name _ _ _) = M.insert name cmd m

-- | Largely delegate events to the editor widget, but also handles some completion tasks
--
-- The additional features over the default editor widget include:
--
--  * Completion of commands (unique commands can be completed with <tab>, while
--    multiple valid completions can be cycled with tab)
--
--  * After a command is selected (with <enter>), the minibuffer will prompt for
--    the required number of arguments
--
--  * Pressing <C-g> while a command is processing arguments or waiting for a
--    command will deactivate the minibuffer
handleMinibufferEvent :: (Ord n, Eq t, Monoid t, Z.GenericTextZipper t)
                      => V.Event
                      -> Minibuffer t n
                      -> B.EventM n (Minibuffer t n)
handleMinibufferEvent evt mb =
  case evt of
    V.EvKey V.KEnter [] ->
      case state mb of
        Editing -> do
          -- If we are waiting for a command, try to accept the command and start
          -- processing arguments
          let val = Z.toList (mconcat (B.getEditContents (editor mb)))
          case M.lookup (T.pack val) (commandIndex mb) of
            Nothing -> return mb
            Just (Command _ argNames argTypes callback) ->
              return mb { state = CollectingArguments argNames argTypes PL.Nil PL.Nil argTypes callback
                        , editor = B.applyEdit (const (Z.textZipper [] Nothing)) (editor mb)
                        }
        CollectingArguments expectedArgNames expectedArgTypes collectedArgTypes collectedArgValues callbackType callback ->
          case (expectedArgNames, expectedArgTypes) of
            (PL.Nil, PL.Nil) ->
              withReversedF collectedArgTypes collectedArgValues $ \collectedArgTypes' collectedArgValues' -> do
                case () of
                  _ | Just Refl <- testEquality callbackType collectedArgTypes' -> do
                        liftIO (callback collectedArgValues')
                        return mb { state = Editing
                                  , editor = B.applyEdit (const (Z.textZipper [] Nothing)) (editor mb)
                                  }
                    | otherwise -> error "impossible"
            (C.Const expectedArgName PL.:< restArgs, expectedArgType PL.:< restTypes) ->
              case expectedArgType of
                StringTypeRepr -> do
                  let val = Z.toList (mconcat (B.getEditContents (editor mb)))
                  return mb { state = CollectingArguments restArgs restTypes (StringTypeRepr PL.:< collectedArgTypes) (StringArgument (T.pack val) PL.:< collectedArgValues) callbackType callback
                            , editor = B.applyEdit (const (Z.textZipper [] Nothing)) (editor mb)
                            }
    V.EvKey (V.KChar '\t') [] ->
      -- If there is a single match, replace the editor contents with it.
      -- Otherwise, do nothing.
      case matchedCommands mb of
        (Command ctxt _ _ _) Seq.:<| Seq.Empty -> do
          let str = T.unpack ctxt
          let chars = map Z.singleton str
          return mb { editor = B.applyEdit (const (Z.textZipper [mconcat chars] Nothing)) (editor mb)
                    }
        _ -> return mb
    V.EvKey (V.KChar 'n') [V.MCtrl] ->
      -- Select the next match in the completion list
      undefined
    V.EvKey (V.KChar 'p') [V.MCtrl] ->
      -- Select the previous match in the completion list
      undefined
    V.EvKey (V.KChar 'g') [V.MCtrl] ->
      -- Cancel everything and reset to a base state (including an empty editor line)
      return Minibuffer { prefix = prefix mb
                        , editor = B.applyEdit (const (Z.textZipper [] Nothing)) (editor mb)
                        , allCommands = allCommands mb
                        , commandIndex = commandIndex mb
                        , matchedCommands = Seq.empty
                        , selectedMatch = Nothing
                        , state = Editing
                        }
    _ -> do
      editor' <- B.handleEditorEvent evt (editor mb)
      let val = Z.toList (mconcat (B.getEditContents editor'))
      let reStr = L.intercalate ".*" (map RE.escapeREString (words val))
      case RE.compileRegex reStr of
        Nothing -> return mb { editor = editor' }
        Just re -> do
          let matches = Seq.filter (commandMatches re) (allCommands mb)
          return mb { editor = editor'
                    , matchedCommands = matches
                    , selectedMatch = fmap (\ix -> if ix >= Seq.length matches then 0 else ix) (selectedMatch mb)
                    }

withReversedF :: forall a b c tps . PL.List a tps -> PL.List b tps -> (forall tps' . PL.List a tps' -> PL.List b tps' -> c) -> c
withReversedF l1 l2 k = go PL.Nil PL.Nil l1 l2
  where
    go :: PL.List a tps0 -> PL.List b tps0 -> PL.List a tps1 -> PL.List b tps1 -> c
    go acc1 acc2 lst1 lst2 =
      case (lst1, lst2) of
        (PL.Nil, PL.Nil) -> k acc1 acc2
        (x1 PL.:< xs1, x2 PL.:< xs2) -> go (x1 PL.:< acc1) (x2 PL.:< acc2) xs1 xs2

commandMatches :: RE.RE -> Command -> Bool
commandMatches rx (Command name _ _ _) = RE.matched (name RE.?=~ rx)

renderMinibuffer :: (Ord n, Show n, B.TextWidth t, Z.GenericTextZipper t)
                 => Bool
                 -> Minibuffer t n
                 -> B.Widget n
renderMinibuffer hasFocus mb = B.renderEditor (drawContent mb) hasFocus (editor mb)

drawContent :: Minibuffer t n -> [t] -> B.Widget n
drawContent = undefined
