{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeInType #-}
module Surveyor.Core.Keymap (
  Key(..),
  Keymap,
  emptyKeymap,
  addGlobalKey,
  lookupKeyCommand
  ) where

import qualified Data.Map as M
import qualified Graphics.Vty as V

import qualified Surveyor.Core.Command as C

data Key = Key V.Key [V.Modifier]
  deriving (Eq, Ord, Show)

data Keymap b m where
  Keymap :: !(M.Map Key (C.SomeCommand b)) -- Some (C.Command b)))
         -> !(M.Map m (M.Map Key (C.SomeCommand b))) --  (C.Command b))))
         -> Keymap b m

-- | Create a new empty keymap
emptyKeymap :: Keymap b m
emptyKeymap = Keymap M.empty M.empty

-- | Add a command to the global keymap
addGlobalKey :: forall b m (tps :: [C.ArgumentKind b]) . (C.CommandLike b) => Key -> C.Command b tps -> Keymap b m -> Keymap b m
addGlobalKey k cmd (Keymap gks mks) =
  Keymap (M.insert k (C.SomeCommand cmd) gks) mks

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
