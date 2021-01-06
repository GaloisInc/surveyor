{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
-- | This module defines a default keymap that should be used when initializing the UI
module Surveyor.Brick.Keymap ( defaultKeymap ) where

import qualified Data.Foldable as F
import           Data.Kind ( Type )
import qualified Data.Parameterized.Nonce as PN
import qualified Graphics.Vty as V
import qualified Surveyor.Core as C

import qualified Surveyor.Brick.Command as SBC
import qualified Surveyor.Brick.Extension as SBE

-- | A default keymap with some reasonable keybindings
--
-- This includes keybindings for a number of different widgets and contexts.
-- Additional keybindings are added if there is a currently-loaded architecture
-- (witnessed by a nonce).
defaultKeymap :: forall (s :: Type) st arch
               . (st ~ C.S SBE.BrickUIExtension SBE.BrickUIState)
              => Maybe (PN.Nonce s arch)
              -- ^ The nonce for the current architecture, if any
              -> C.Keymap (C.SurveyorCommand s st) (C.SomeUIMode s)
defaultKeymap mNonce =
  F.foldl' addModeKeys k0 allModeKeys
  where
    k0 = addGlobalKey C.emptyKeymap globalKeys
    allModeKeys = concat [ modeKeys
                         , maybe [] archKeys mNonce
                         ]

addGlobalKey :: (F.Foldable t, C.CommandLike b)
             => C.Keymap b m
             -> t (C.Key, C.SomeCommand b)
             -> C.Keymap b m
addGlobalKey km0 maps =
  F.foldl' (\km (k, C.SomeCommand cmd) -> C.addGlobalKey k cmd km) km0 maps

addModeKeys :: (F.Foldable t, C.CommandLike b, Ord a)
            => C.Keymap b a
            -> (a, t (C.Key, C.SomeCommand b))
            -> C.Keymap b a
addModeKeys modeKeymap (mode, keys) =
  F.foldl' (\km (k, C.SomeCommand cmd) -> C.addModeKey mode k cmd km) modeKeymap keys

-- | Mode-specific keys that do not have any dependency on the architecture
modeKeys :: [(C.SomeUIMode s, [(C.Key, C.SomeCommand (C.SurveyorCommand s (C.S SBE.BrickUIExtension SBE.BrickUIState)))])]
modeKeys = [ (C.SomeUIMode C.SymbolicExecutionManager,
                  [ (C.Key (V.KChar 'n') [], C.SomeCommand SBC.promptValueNameC)
                  , (C.Key (V.KChar 's') [], C.SomeCommand C.stepExecutionC)
                  , (C.Key (V.KChar 'c') [], C.SomeCommand C.continueExecutionC)
                  , (C.Key (V.KChar 'i') [], C.SomeCommand C.interruptExecutionC)
                  ])
           ]

-- | Keys available from any context (not associated with any keymap)
globalKeys :: [(C.Key, C.SomeCommand (C.SurveyorCommand s (C.S SBE.BrickUIExtension SBE.BrickUIState)))]
globalKeys =
  [ (C.Key (V.KChar 'q') [V.MCtrl], C.SomeCommand C.exitC)
  , (C.Key (V.KChar 'x') [V.MMeta], C.SomeCommand SBC.minibufferC)
  ]

-- | These keybindings require an architecture to be available (witnessed by a nonce)
archKeys :: PN.Nonce s arch
         -> [(C.SomeUIMode s, [(C.Key, C.SomeCommand (C.SurveyorCommand s (C.S SBE.BrickUIExtension SBE.BrickUIState)))])]
archKeys nonce =
  [ blockViewerKeys nonce C.BaseRepr
  , blockViewerKeys nonce C.MacawRepr
  , blockViewerKeys nonce C.CrucibleRepr
  , functionViewerKeys nonce C.BaseRepr
  , functionViewerKeys nonce C.MacawRepr
  , functionViewerKeys nonce C.CrucibleRepr
  ]

-- | Keybindings for the function viewer
functionViewerKeys :: PN.Nonce s arch
                   -> C.IRRepr arch ir
                   -> (C.SomeUIMode s, [(C.Key, C.SomeCommand (C.SurveyorCommand s (C.S SBE.BrickUIExtension SBE.BrickUIState)))])
functionViewerKeys nonce rep = ( C.SomeUIMode (C.FunctionViewer nonce rep)
                               , [ (C.Key (V.KChar 'm') [], C.SomeCommand SBC.showMacawFunctionC)
                                 , (C.Key (V.KChar 'c') [], C.SomeCommand SBC.showCrucibleFunctionC)
                                 , (C.Key (V.KChar 'b') [], C.SomeCommand SBC.showBaseFunctionC)
                                 , (C.Key (V.KChar 's') [], C.SomeCommand C.initializeSymbolicExecutionC)
                                 ]
                               )

-- | Keybindings for the block viewer
blockViewerKeys :: PN.Nonce s arch
                -> C.IRRepr arch ir
                -> (C.SomeUIMode s, [(C.Key, C.SomeCommand (C.SurveyorCommand s (C.S SBE.BrickUIExtension SBE.BrickUIState)))])
blockViewerKeys nonce rep = ( C.SomeUIMode (C.BlockViewer nonce rep)
                            , [ (C.Key V.KDown [], C.SomeCommand C.selectNextInstructionC)
                              , (C.Key (V.KChar 'n') [V.MCtrl], C.SomeCommand C.selectNextInstructionC)
                              , (C.Key V.KUp [], C.SomeCommand C.selectPreviousInstructionC)
                              , (C.Key (V.KChar 'p') [V.MCtrl], C.SomeCommand C.selectPreviousInstructionC)
                              , (C.Key V.KEsc [], C.SomeCommand C.resetInstructionSelectionC)
                              , (C.Key V.KRight [], C.SomeCommand C.selectNextOperandC)
                              , (C.Key V.KLeft [], C.SomeCommand C.selectPreviousOperandC)
                              , (C.Key (V.KChar 'm') [], C.SomeCommand SBC.showMacawBlockC)
                              , (C.Key (V.KChar 'c') [], C.SomeCommand SBC.showCrucibleBlockC)
                              , (C.Key (V.KChar 'b') [], C.SomeCommand SBC.showBaseBlockC)
                              ]
                            )
