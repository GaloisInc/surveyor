{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Surveyor.Core.Events (
  EventExtension,
  Events(..),
  LoadEvent(..),
  LoggingEvent(..),
  InfoEvent(..),
  SymbolicExecutionEvent(..),
  ContextEvent(..),
  DebuggingEvent(..),
  ToEvent(..),
  emitEvent
  ) where

import qualified Control.Concurrent.Chan as CCC
import qualified Control.Exception as X
import qualified Control.NF as NF
import qualified Data.ElfEdit as E
import qualified Data.Functor.Identity as I
import           Data.Int ( Int64 )
import           Data.Kind ( Type )
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.Nonce as PN
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Text as T
import qualified Lang.Crucible.Backend as CB
import qualified Lang.Crucible.CFG.Core as CCC
import qualified Lang.Crucible.Simulator.Profiling as CSP
import qualified Lang.Crucible.Simulator.RegMap as LMCR
import qualified What4.BaseTypes as WT
import qualified What4.Expr.Builder as WEB

import qualified Renovate as R

import qualified Surveyor.Core.Architecture as A
import qualified Surveyor.Core.Chan as SCC
import qualified Surveyor.Core.Command as C
import qualified Surveyor.Core.IRRepr as IR
import qualified Surveyor.Core.Logging.Message as CLM
import qualified Surveyor.Core.SymbolicExecution.Config as SE
import qualified Surveyor.Core.SymbolicExecution.ExecutionFeature as SCSE
import qualified Surveyor.Core.SymbolicExecution.Override as SEO
import qualified Surveyor.Core.SymbolicExecution.Session as CSS
import qualified Surveyor.Core.SymbolicExecution.State as SES

data LoadEvent s st where
  ErrorLoadingELFHeader :: Int64 -> String -> LoadEvent s st
  ErrorLoadingELF :: [E.ElfParseError] -> LoadEvent s st
  ErrorLoadingLLVM :: String -> LoadEvent s st
  AnalysisFailure :: X.SomeException -> LoadEvent s st
  AnalysisFinished :: A.SomeResult s arch -> [R.Diagnostic] -> LoadEvent s st
  AnalysisProgress :: A.SomeResult s arch -> LoadEvent s st

  LoadELF :: FilePath -> LoadEvent s st
  LoadLLVM :: FilePath -> LoadEvent s st
  LoadJAR :: FilePath -> LoadEvent s st
  -- | Attempt to load a file by detecting its type automatically
  LoadFile :: FilePath -> LoadEvent s st

data LoggingEvent s st where
  -- | Send a log message to the system
  --
  -- It will be accumulated in an internal buffer, which can be viewed in a
  -- UI-specific manner
  LogDiagnostic :: !(CLM.Timestamped CLM.LogMessage) -> LoggingEvent s st
  -- | Set the logger (i.e., 'LogDiagnostic') to log to a file as well as the
  -- internal buffer; if there was already a log file, the handler for this
  -- event should close the existing file and replace the file logger with a new
  -- one.
  SetLogFile :: FilePath -> LoggingEvent s st
  -- | Turn off logging to a file, performing any necessary cleanup
  DisableFileLogging :: LoggingEvent s st

-- | Events allowing the user to request information
data InfoEvent s st where
  -- | Show a description (help text and expected arguments) of a command in a
  -- manner suitable for the UI
  DescribeCommand :: (C.CommandLike cmd) => C.SomeCommand cmd -> InfoEvent s st
  -- | Display a description of the keybindings active in the current context in
  -- a manner suitable for the UI
  DescribeKeys :: InfoEvent s st
  -- | Convert the given symbolic value using graphviz dot format
  --
  -- If a 'FilePath' is provided, save it to that file; otherwise, visualize it
  -- with the default platform dot viewer
  VisualizeSymbolicTerm :: (sym ~ WEB.ExprBuilder s state fs) => LMCR.RegEntry sym tp -> Maybe FilePath -> InfoEvent s st

-- | Events related to controlling the symbolic execution engine/state
data SymbolicExecutionEvent s st where
  -- | Begin setting up symbolic execution for the provided function.
  --
  -- If the function handle argument is not provided, set up symbolic execution
  -- for the current function.  Entering this state discards any previous
  -- symbolic execution state in the current context.
  InitializeSymbolicExecution :: PN.Nonce s arch
                              -> Maybe (SE.SymbolicExecutionConfig s)
                              -> Maybe (A.FunctionHandle arch s)
                              -> SymbolicExecutionEvent s st
  BeginSymbolicExecutionSetup :: PN.Nonce s arch
                              -> SE.SymbolicExecutionConfig s
                              -> CCC.SomeCFG (A.CrucibleExt arch) init reg
                              -> SymbolicExecutionEvent s st
  StartSymbolicExecution :: (CB.IsSymInterface sym, sym ~ WEB.ExprBuilder s state fs)
                         => PN.Nonce s arch
                         -> A.AnalysisResult arch s
                         -> SES.SymbolicState arch s sym init reg
                         -> Ctx.Assignment (LMCR.RegEntry sym) init
                         -> SymbolicExecutionEvent s st
  ReportSymbolicExecutionMetrics :: CSS.SessionID s -> CSP.Metrics I.Identity -> SymbolicExecutionEvent s st
  -- | Prompt the user for the name for the currently-selected value (which is
  -- determined by the current context)
  InitializeValueNamePrompt :: PN.Nonce s (arch :: Type) -> T.Text -> SymbolicExecutionEvent s st
  -- | Update the state with the name for a value
  NameValue :: PN.Nonce s (tp :: WT.BaseType) -> T.Text -> SymbolicExecutionEvent s st
  -- | Update a pre-existing symbolic execution state
  --
  -- While this could be accomplished with asynchronous state update events,
  -- splitting these changes out gives frontends a chance to process the event
  -- and update UI elements as-needed.
  UpdateSymbolicExecutionState :: PN.Nonce s arch -> SES.SymbolicExecutionState arch s k -> SymbolicExecutionEvent s st
  -- | Set the "current" value in the symbolic execution context (for suspended
  -- symbolic execution states)
  --
  -- The current value is used for a few commands.  The frontends can provide
  -- different mechanisms for updating this value.
  SetCurrentSymbolicExecutionValue :: PN.Nonce s (arch :: Type) -> PN.Nonce s (sym :: Type) -> CSS.SessionID s -> Maybe (Some (LMCR.RegEntry sym)) -> SymbolicExecutionEvent s st
  -- | Go back by one step in the symbolic execution trace (if any)
  SymbolicStateBack :: PN.Nonce s (arch :: Type) -> CSS.SessionID s -> SymbolicExecutionEvent s st
  -- | Go forward (towards the actual suspended state) by one step in the symbolic execution trace (if any)
  SymbolicStateForward :: PN.Nonce s (arch :: Type) -> CSS.SessionID s -> SymbolicExecutionEvent s st
  -- | An event generated by the execution feature while it is monitoring states
  --
  -- The message includes the return channel to send responses on, as well as
  -- the configuration variable controlling the behavior of the monitor.
  DebugMonitorEvent :: ( ext ~ A.CrucibleExt arch
                       , sym ~ WEB.ExprBuilder s solver fs
                       , CB.IsSymInterface sym
                       )
                    => PN.Nonce s arch
                    -> CSS.SessionID s
                    -> SCSE.CrucibleExecState s p sym ext
                    -> CCC.Chan (SCSE.ReturnExecState s p sym ext)
                    -> SCSE.DebuggerStateRef p sym ext
                    -> SymbolicExecutionEvent s st
  -- | An event generated from debugging overrides that pause execution to wait for the debugger
  --
  -- The event includes the channel to return a response on
  OverrideMonitorEvent :: ( ext ~ A.CrucibleExt arch
                          , sym ~ WEB.ExprBuilder s solver fs
                          , CB.IsSymInterface sym
                          )
                       => PN.Nonce s arch
                       -> CSS.SessionID s
                       -> SEO.CrucibleSimState s p sym ext
                       -> CCC.Chan (SEO.ReturnSimState s p sym ext)
                       -> SCSE.DebuggerStateRef p sym ext
                       -> SymbolicExecutionEvent s st

-- | Events that manipulate the current context or context stack
data ContextEvent s st where
  -- Context manipulation
  PushContext :: (A.ArchConstraints ir s) => PN.Nonce s arch -> A.FunctionHandle arch s -> IR.IRRepr arch ir -> A.Block ir s -> ContextEvent s st
  ContextBack :: ContextEvent s st
  ContextForward :: ContextEvent s st


  -- Function-related events
  ViewFunction :: PN.Nonce s (arch :: Type) -> IR.IRRepr arch ir -> ContextEvent s st


  -- Block-related events
  ViewBlock :: PN.Nonce s arch -> IR.IRRepr arch ir -> ContextEvent s st
  ViewInstructionSemantics :: PN.Nonce s arch -> ContextEvent s st
  SelectNextInstruction :: PN.Nonce s (arch :: Type) -> ContextEvent s st
  SelectPreviousInstruction :: PN.Nonce s (arch :: Type) -> ContextEvent s st
  SelectNextOperand :: PN.Nonce s (arch :: Type) -> ContextEvent s st
  SelectPreviousOperand :: PN.Nonce s (arch :: Type) -> ContextEvent s st
  ResetInstructionSelection :: PN.Nonce s (arch :: Type) -> ContextEvent s st

-- | Events relating to the debugger
data DebuggingEvent s st where
  -- | Step the given symbolic execution session by one step
  StepExecution :: CSS.SessionID s -> DebuggingEvent s st
  -- | Continue execution until the next return, stepping out of the current function context
  StepOutExecution :: CSS.SessionID s -> DebuggingEvent s st
  -- | Continue the given symbolic execution session
  ContinueExecution :: CSS.SessionID s ->  DebuggingEvent s st
  -- | Interrupt the given symbolic execution, stopping it at the next
  -- crucible hook point
  InterruptExecution :: CSS.SessionID s ->  DebuggingEvent s st
  -- | Enable symbolic execution state collection
  EnableRecording :: CSS.SessionID s -> DebuggingEvent s st
  -- | Disable symbolic execution state collection
  DisableRecording :: CSS.SessionID s -> DebuggingEvent s st

type family EventExtension (st :: Type -> Type -> Type) :: Type -> (Type -> Type -> Type) -> Type

-- | All of the events supported by Surveyor (with extensions for UI-specific events)
data Events s st where
  LoadEvent :: LoadEvent s st -> Events s st
  LoggingEvent :: LoggingEvent s st -> Events s st
  SymbolicExecutionEvent :: SymbolicExecutionEvent s st -> Events s st
  InfoEvent :: InfoEvent s st -> Events s st
  ContextEvent :: ContextEvent s st -> Events s st
  DebuggingEvent :: DebuggingEvent s st -> Events s st

  -- | Apply an arbitrary state update based on some value that has been computed
  -- asynchronously (to avoid blocking the event loop with an expensive
  -- computation).
  AsyncStateUpdate :: PN.Nonce s arch -> !(NF.NF a) -> (a -> st arch s -> st arch s) -> Events s st

  -- | Exit surveyor
  Exit :: Events s st

  -- | UI-specific events
  ExtensionEvent :: EventExtension st s st -> Events s st

-- | This is an ugly ad-hoc class to provide sugar to transparently lift the
-- various sub-event types into 'Events'
class ToEvent s st e where
  toEvent :: e s st -> Events s st

instance ToEvent s st Events where
  toEvent = id

instance ToEvent s st ContextEvent where
  toEvent = ContextEvent

instance ToEvent s st DebuggingEvent where
  toEvent = DebuggingEvent

instance ToEvent s st LoggingEvent where
  toEvent = LoggingEvent

instance ToEvent s st InfoEvent where
  toEvent = InfoEvent

instance ToEvent s st LoadEvent where
  toEvent = LoadEvent

instance ToEvent s st SymbolicExecutionEvent where
  toEvent = SymbolicExecutionEvent

-- | A wrapper around 'SCC.writeChan' that lifts sub-events into the full 'Events' type
emitEvent :: (ToEvent s st e) => SCC.Chan (Events s st) -> e s st -> IO ()
emitEvent c e = SCC.writeChan c (toEvent e)
