{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
module Surveyor.Core (
  -- * Channel abstraction
  CS.Chan,
  CS.mkChan,
  CS.readChan,
  CS.writeChan,
  -- * Events
  CE.Events(..),
  -- * Binaries
  -- ** Loading
  CL.AsyncLoader,
  CL.cancelLoader,
  CL.asynchronouslyLoad,
  CL.asynchronouslyLoadElf,
  CL.asynchronouslyLoadJAR,
  CL.asynchronouslyLoadLLVM,
  -- ** Program representation
  CA.AnalysisResult,
  CA.ArchConstraints,
  CA.IR(..),
  IR.IRRepr(..),
  CA.SomeIRRepr(..),
  CA.Architecture(..),
  CA.Block(..),
  CA.BlockMapping(..),
  CA.FunctionHandle(..),
  CA.ParameterizedFormula(..),
  CA.prettyParameterizedFormula,
  CA.SomeResult(..),
  CA.OperandList(..),
  CA.OperandListItem(..),
  CA.indexOperandList,
  CA.Delimiter(..),
  CA.Zipper,
  CA.zipper,
  CA.zipperNext,
  CA.zipperPrev,
  CA.zipperFocused,
  -- * State
  module Surveyor.Core.State,
  CAS.asynchronously,
  -- ** Modes
  M.UIMode(..),
  M.UIKind,
  M.NormalK,
  M.MiniBufferK,
  M.SomeUIMode(..),
  M.prettyMode,
  -- * Completion system
  AR.SurveyorCommand,
  CC.Command(..),
  CC.SomeCommand(..),
  CC.CommandLike(..),
  AR.Argument(..),
  AR.SomeAddress(..),
  AR.SomeState(..),
  -- ** Commands
  module Surveyor.Core.Commands,
  -- ** Types and type representatives for the completion system
  AR.HasNonce(..),
  AR.SomeNonce(..),
  AR.Type(..),
  AR.TypeRepr(..),
  AR.IntType,
  AR.WordType,
  AR.AddressType,
  AR.StringType,
  AR.CommandType,
  AR.FilePathType,
  AR.showRepr,
  AR.parseArgument,
  -- * Context
  CCX.Context(..),
  CCX.InstructionSelection(..),
  CCX.ContextStack(..),
  CCX.BlockState(..),
  CCX.emptyContextStack,
  CCX.makeContext,
  CCX.selectedIndex,
  -- ** Modifiers
  CCX.resetBlockSelection,
  CCX.selectNextInstruction,
  CCX.selectPreviousInstruction,
  CCX.selectNextOperand,
  CCX.selectPreviousOperand,
  CCX.selectNextBlock,
  CCX.selectPreviousBlock,
  CCX.pushContext,
  CCX.contextBack,
  CCX.contextForward,
  -- ** Lenses
  CCX.currentContext,
  CCX.contextFocusedBlock,
  CCX.blockStateFor,
  CCX.blockStateList,
  CCX.blockStateBlock,
  CCX.blockStateSelection,
  CCX.cfgG,
  CCX.vertexMapG,
  CCX.selectedBlockL,
  -- * Translation Cache
  TC.TranslationCache,
  TC.newTranslationCache,
  -- * The EchoArea abstraction
  EA.EchoArea,
  EA.echoArea,
  EA.getEchoAreaText,
  EA.setEchoAreaText,
  EA.resetEchoArea,
  -- * Keymap
  K.Keymap,
  K.Key(..),
  K.emptyKeymap,
  K.addGlobalKey,
  K.addModeKey,
  K.modeKeybindings,
  K.lookupKeyCommand,
  -- * Logging
  logMessage,
  logDiagnostic,
  CE.LogLevel(..),
  -- * Defaults
  defaultKeymap
  ) where

import qualified Data.Foldable as F
import qualified Graphics.Vty as V

import qualified Surveyor.Core.Architecture as CA
import qualified Surveyor.Core.Arguments as AR
import qualified Surveyor.Core.Async as CAS
import qualified Surveyor.Core.Chan as CS
import qualified Surveyor.Core.Command as CC
import           Surveyor.Core.Commands
import qualified Surveyor.Core.Context as CCX
import qualified Surveyor.Core.EchoArea as EA
import qualified Surveyor.Core.Events as CE
import qualified Surveyor.Core.IRRepr as IR
import qualified Surveyor.Core.Keymap as K
import qualified Surveyor.Core.Loader as CL
import qualified Surveyor.Core.Mode as M
import           Surveyor.Core.State
import qualified Surveyor.Core.TranslationCache as TC

-- | A default keymap with some reasonable keybindings
defaultKeymap :: forall (s :: *) e u . K.Keymap (AR.SurveyorCommand s (S e u)) (M.SomeUIMode s)
defaultKeymap = F.foldl' (\km (k, CC.SomeCommand cmd) -> K.addGlobalKey k cmd km) K.emptyKeymap globals
  where
    globals = [ (K.Key (V.KChar 'q') [V.MCtrl], CC.SomeCommand exitC)
              , (K.Key (V.KChar 'x') [V.MMeta], CC.SomeCommand minibufferC)
              ]
