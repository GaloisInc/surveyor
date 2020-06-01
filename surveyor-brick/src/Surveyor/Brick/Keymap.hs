{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
module Surveyor.Brick.Keymap ( defaultKeymap ) where

import qualified Data.Foldable as F
import           Data.Kind ( Type )
import qualified Graphics.Vty as V
import qualified Surveyor.Core as C

import qualified Surveyor.Brick.Command as SBC
import qualified Surveyor.Brick.Extension as SBE

-- | A default keymap with some reasonable keybindings
defaultKeymap :: forall (s :: Type) st . (st ~ C.S SBE.BrickUIExtension SBE.BrickUIState) => C.Keymap (C.SurveyorCommand s st) (C.SomeUIMode s)
defaultKeymap = F.foldl' (\km (k, C.SomeCommand cmd) -> C.addGlobalKey k cmd km) C.emptyKeymap globals
  where
    globals = [ (C.Key (V.KChar 'q') [V.MCtrl], C.SomeCommand C.exitC)
              , (C.Key (V.KChar 'x') [V.MMeta], C.SomeCommand SBC.minibufferC)
              ]
