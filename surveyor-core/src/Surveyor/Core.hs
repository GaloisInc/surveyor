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
  CA.CruciblePersonality,
  -- ** Operand lists
  OL.OperandList,
  OL.OperandListItem(..),
  OL.indexOperandList,
  OL.Delimiter(..),
  OL.Zipper,
  OL.zipper,
  OL.zipperNext,
  OL.zipperPrev,
  OL.zipperFocused,
  OL.listItems,
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
  CC.ArgumentKind,
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
  -- ** Symbolic Execution
  SymEx.Solver(..),
  SymEx.SymbolicExecutionConfig,
  SymEx.SomeFloatModeRepr(..),
  SymEx.defaultSymbolicExecutionConfig,
  SymEx.SymbolicState(..),
  SymEx.SessionState,
  SymEx.lookupSessionState,
  SymEx.singleSessionState,
  SymEx.updateSessionMetrics,
  SymEx.SymbolicExecutionState(..),
  SymEx.Config,
  SymEx.SetupArgs,
  SymEx.Execute,
  SymEx.Inspect,
  SymEx.ExecutionProgress,
  SymEx.executionMetrics,
  SymEx.symbolicExecutionConfig,
  SymEx.configuringSymbolicExecution,
  SymEx.initializingSymbolicExecution,
  SymEx.startSymbolicExecution,
  SymEx.cleanupSymbolicExecutionState,
  SymEx.configSolverL,
  SymEx.configFloatReprL,
  SymEx.solverInteractionFileL,
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
  CCX.symExecSessionIDL,
  CCX.baseFunctionG,
  CCX.currentContext,
  CCX.blockStateFor,
  CCX.blockStateList,
  CCX.blockStateBlock,
  CCX.blockStateSelection,
  CCX.cfgG,
  CCX.vertexMapG,
  CCX.selectedBlockL,
  CCX.functionStateFor,
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
import           Data.Kind ( Type )
import qualified Graphics.Vty as V

import qualified Surveyor.Core.Architecture as CA
import qualified Surveyor.Core.Arguments as AR
import qualified Surveyor.Core.Async as CAS
import qualified Surveyor.Core.Chan as CS
import qualified Surveyor.Core.Command as CC
import           Surveyor.Core.Commands
import qualified Surveyor.Core.Context as CCX
import qualified Surveyor.Core.Context.SymbolicExecution as SymEx
import qualified Surveyor.Core.EchoArea as EA
import qualified Surveyor.Core.Events as CE
import qualified Surveyor.Core.IRRepr as IR
import qualified Surveyor.Core.Keymap as K
import qualified Surveyor.Core.Loader as CL
import qualified Surveyor.Core.Mode as M
import qualified Surveyor.Core.OperandList as OL
import           Surveyor.Core.State
import qualified Surveyor.Core.TranslationCache as TC

-- | A default keymap with some reasonable keybindings
defaultKeymap :: forall (s :: Type) e u . K.Keymap (AR.SurveyorCommand s (S e u)) (M.SomeUIMode s)
defaultKeymap = F.foldl' (\km (k, CC.SomeCommand cmd) -> K.addGlobalKey k cmd km) K.emptyKeymap globals
  where
    globals = [ (K.Key (V.KChar 'q') [V.MCtrl], CC.SomeCommand exitC)
              , (K.Key (V.KChar 'x') [V.MMeta], CC.SomeCommand minibufferC)
              ]
