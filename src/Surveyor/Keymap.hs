{-# LANGUAGE PolyKinds #-}
module Surveyor.Keymap (
  Key(..),
  Keymap,
  emptyKeymap,
  addGlobalKey,
  lookupKeyCommand
  ) where

import qualified Data.Map as M
import           Data.Parameterized.Some ( Some(..) )
import qualified Graphics.Vty as V

import qualified Surveyor.Minibuffer as MB

data Key = Key V.Key [V.Modifier]
  deriving (Eq, Ord, Show)

data Keymap m a r =
  Keymap { globalKeys :: !(M.Map Key (Some (MB.Command a r)))
         , modeKeys :: !(M.Map m (M.Map Key (Some (MB.Command a r))))
         }

-- | Create a new empty keymap
emptyKeymap :: Keymap m a r
emptyKeymap = Keymap { globalKeys = M.empty
                     , modeKeys = M.empty
                     }

-- | Add a command to the global keymap
addGlobalKey :: Key -> Some (MB.Command a r) -> Keymap m a r -> Keymap m a r
addGlobalKey k cmd m =
  m { globalKeys = M.insert k cmd (globalKeys m) }

-- | Look up the command for the given key, if any
--
-- The map for the given mode is checked first; if there is no relevant
-- keybinding, the global keymap is also consulted.
lookupKeyCommand :: (Ord m)
                 => m
                 -> Key
                 -> Keymap m a r
                 -> Maybe (Some (MB.Command a r))
lookupKeyCommand mode k km =
  case M.lookup mode (modeKeys km) of
    Nothing -> M.lookup k (globalKeys km)
    Just modeMap ->
      case M.lookup k modeMap of
        Nothing -> M.lookup k (globalKeys km)
        Just cmd -> Just cmd
