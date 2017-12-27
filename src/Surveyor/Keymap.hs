module Surveyor.Keymap ( defaultKeymap ) where

import qualified Brick.BChan as B
import qualified Data.Foldable as F
import           Data.Parameterized.Some ( Some(..) )
import qualified Graphics.Vty as V

import qualified Brick.Keymap as K
import qualified Surveyor.Commands as C
import           Surveyor.Events
import qualified Surveyor.Minibuffer as MB
import           Surveyor.Mode

-- | A default keymap with some reasonable keybindings
defaultKeymap :: B.BChan (Events s) -> K.Keymap SomeUIMode (MB.Argument arch s) MB.TypeRepr
defaultKeymap c = F.foldl' (\km (k, cmd) -> K.addGlobalKey k cmd km) K.emptyKeymap globals
  where
    globals = [ (K.Key (V.KChar 'q') [V.MCtrl], Some (C.exitC c))
              , (K.Key (V.KChar 'x') [V.MMeta], Some (C.minibufferC c))
              ]