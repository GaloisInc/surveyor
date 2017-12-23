module Surveyor.Keymap (
  Key(..),
  Keymap,
  emptyKeymap,
  addGlobalKey,
  lookupKeyCommand,
  defaultKeymap
  ) where

import qualified Brick.BChan as B
import qualified Data.Foldable as F
import qualified Data.Map as M
import           Data.Parameterized.Some ( Some(..) )
import qualified Graphics.Vty as V

import qualified Surveyor.Commands as C
import           Surveyor.Events
import qualified Surveyor.Minibuffer as MB
import           Surveyor.Mode

data Key = Key V.Key [V.Modifier]
  deriving (Eq, Ord, Show)

data Keymap = Keymap { globalKeys :: !(M.Map Key (Some (MB.Command MB.Argument MB.TypeRepr)))
                     , modeKeys :: !(M.Map SomeUIMode (M.Map Key (Some (MB.Command MB.Argument MB.TypeRepr))))
                     }

-- | Create a new empty keymap
emptyKeymap :: Keymap
emptyKeymap = Keymap { globalKeys = M.empty
                     , modeKeys = M.fromList [ (SomeUIMode Diags, M.empty)
                                             , (SomeUIMode Summary, M.empty)
                                             , (SomeUIMode BlockSelector, M.empty)
                                             ]
                     }

-- | Add a command to the global keymap
addGlobalKey :: Key -> Some (MB.Command MB.Argument MB.TypeRepr) -> Keymap -> Keymap
addGlobalKey k cmd m =
  m { globalKeys = M.insert k cmd (globalKeys m) }

-- | Look up the command for the given key, if any
--
-- The map for the given mode is checked first; if there is no relevant
-- keybinding, the global keymap is also consulted.
lookupKeyCommand :: SomeUIMode
                 -> Key
                 -> Keymap
                 -> Maybe (Some (MB.Command MB.Argument MB.TypeRepr))
lookupKeyCommand mode k km =
  case M.lookup mode (modeKeys km) of
    Nothing -> M.lookup k (globalKeys km)
    Just modeMap ->
      case M.lookup k modeMap of
        Nothing -> M.lookup k (globalKeys km)
        Just cmd -> Just cmd

-- | A default keymap with some reasonable keybindings
defaultKeymap :: B.BChan (Events s) -> Keymap
defaultKeymap c = F.foldl' (\km (k, cmd) -> addGlobalKey k cmd km) emptyKeymap globals
  where
    globals = [ (Key (V.KChar 'q') [V.MCtrl], Some (C.exitC c))
              ]
