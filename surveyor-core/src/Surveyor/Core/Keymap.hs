{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeInType #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Surveyor.Core.Keymap (
  Key(..),
  Keymap,
  emptyKeymap,
  addGlobalKey,
  addModeKey,
  modeKeybindings,
  lookupKeyCommand
  ) where

import qualified Data.Map as M
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Graphics.Vty as V

import qualified Surveyor.Core.Command as C

data Key = Key V.Key [V.Modifier]
  deriving (Eq, Ord, Show)

instance PP.Pretty Key where
  pretty (Key k ms) =
    case ms of
      [] -> PP.pretty k
      _ -> mconcat (map PP.pretty ms) <> PP.pretty k

instance PP.Pretty V.Modifier where
  pretty m =
    case m of
      V.MAlt -> "M-"
      V.MCtrl -> "C-"
      V.MMeta -> "M-"
      V.MShift -> "S-"

instance PP.Pretty V.Key where
  pretty k =
    case k of
      V.KChar c -> PP.pretty c
      V.KUp -> "Up"
      V.KDown -> "Down"
      V.KLeft -> "Left"
      V.KRight -> "Right"
      V.KEsc -> "Esc"
      V.KEnter -> "Enter"
      V.KPageUp -> "PgUp"
      V.KPageDown -> "PgDn"
      V.KHome -> "Home"
      V.KEnd -> "End"
      _ -> PP.pretty (show k)

data Keymap b m where
  Keymap :: !(M.Map Key (C.SomeCommand b))
         -> !(M.Map m (M.Map Key (C.SomeCommand b)))
         -> Keymap b m

-- | Create a new empty keymap
emptyKeymap :: Keymap b m
emptyKeymap = Keymap M.empty M.empty

-- | Add a command to the global keymap
addGlobalKey :: forall b m (tps :: [C.ArgumentKind b])
              . (C.CommandLike b)
             => Key
             -> C.Command b tps
             -> Keymap b m
             -> Keymap b m
addGlobalKey k cmd (Keymap gks mks) =
  Keymap (M.insert k (C.SomeCommand cmd) gks) mks

addModeKey :: forall b m (tps :: [C.ArgumentKind b])
            . (C.CommandLike b, Ord m)
           => m
           -> Key
           -> C.Command b tps
           -> Keymap b m
           -> Keymap b m
addModeKey mode k cmd (Keymap gks mks) =
  Keymap gks (M.alter addKey mode mks)
  where
    addKey Nothing = Just (M.singleton k (C.SomeCommand cmd))
    addKey (Just mm) = Just (M.insert k (C.SomeCommand cmd) mm)

modeKeybindings :: (Ord m) => Keymap b m -> m -> [(Key, C.SomeCommand b)]
modeKeybindings (Keymap _gks mks) m =
  maybe [] M.toList (M.lookup m mks)

-- | Look up the command for the given key, if any
--
-- The map for the given mode is checked first; if there is no relevant
-- keybinding, the global keymap is also consulted.
lookupKeyCommand :: (Ord m)
                 => m
                 -> Key
                 -> Keymap b m
                 -> Maybe (C.SomeCommand b)
lookupKeyCommand mode k (Keymap gks mks) =
  case M.lookup mode mks of
    Nothing -> M.lookup k gks
    Just modeMap ->
      case M.lookup k modeMap of
        Nothing -> M.lookup k gks
        Just cmd -> Just cmd
