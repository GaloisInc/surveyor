module Surveyor.Keymap ( defaultKeymap ) where

import qualified Data.Foldable as F
import qualified Data.Parameterized.Nonce as NG
import           Data.Parameterized.Some ( Some(..) )
import qualified Graphics.Vty as V

import qualified Surveyor.Core.Keymap as K
import qualified Surveyor.Arguments as AR
import qualified Surveyor.Commands as C
import           Surveyor.Events
import           Surveyor.Mode

-- | A default keymap with some reasonable keybindings
defaultKeymap :: K.Keymap (Events s) SomeUIMode (Maybe (NG.Nonce s arch)) (AR.Argument arch (Events s) (Maybe (NG.Nonce s arch)) s) AR.TypeRepr
defaultKeymap = F.foldl' (\km (k, cmd) -> K.addGlobalKey k cmd km) K.emptyKeymap globals
  where
    globals = [ (K.Key (V.KChar 'q') [V.MCtrl], Some C.exitC)
              , (K.Key (V.KChar 'x') [V.MMeta], Some C.minibufferC)
              ]
