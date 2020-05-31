{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module Surveyor.Core.State (
  State(..),
  LoggingActions(..),
  S(..),
  ArchState(..),
  AppState(..),
  sEmitEvent,
  -- * Logging
  logMessage,
  -- * Lenses
  lStateLogger,
  lFileLogger,
  lLogActions,
  lInputFile,
  lLoader,
  lLogStore,
  diagnosticLevelL,
  lEchoArea,
  lUIMode,
  lAppState,
  lEventChannel,
  lNonceGenerator,
  lUIExtension,
  lArchState,
  lNonce,
  lAnalysisResult,
  lKeymap,
  lUIState,
  contextL,
  contextG,
  symExStateL,
  irCacheL
  ) where

import           GHC.Generics ( Generic )

import qualified Control.Concurrent.Async as CA
import qualified Control.Lens as L
import qualified Data.Generics.Product as GL
import           Data.Kind ( Type )
import qualified Data.Parameterized.Nonce as NG

import qualified Surveyor.Core.Architecture as A
import qualified Surveyor.Core.Arguments as AR
import qualified Surveyor.Core.Chan as C
import qualified Surveyor.Core.Context as CC
import qualified Surveyor.Core.SymbolicExecution as SE
import qualified Surveyor.Core.EchoArea as EA
import qualified Surveyor.Core.Events as SCE
import           Surveyor.Core.Keymap ( Keymap )
import           Surveyor.Core.Loader ( AsyncLoader )
import qualified Surveyor.Core.Logging as SCL
import           Surveyor.Core.Mode
import qualified Surveyor.Core.TranslationCache as TC

-- | This is a wrapper around the state type suitable for the UI core.  It hides
-- the architecture so that the architecture can change during run-time (i.e.,
-- when a new binary is loaded).
data State e u s where
  State :: (A.Architecture arch s) => !(S e u arch s) -> State e u s

instance AR.HasNonce (S e u) where
  getNonce (AR.SomeState s) = AR.SomeNonce (sArchNonce s)

-- | There are multiple supported logging actions: internal state and file
--
-- We keep them separate so that the file logger can be replaced or redirected
-- to a different file at run-time.  If we pre-combine them with the monoid
-- instance for LogAction, we cannot separate them later.
data LoggingActions =
  LoggingActions { sStateLogger :: SCL.LogAction
                 -- ^ The logger that logs to an internal buffer (for display in the frontend UI)
                 , sFileLogger :: Maybe (CA.Async (), SCL.LogAction)
                 -- ^ The optional logger to a file target, along with the async thread
                 -- that implements the file logger.  If the file logger is replaced,
                 -- the async thread should be canceled.
                 }
  deriving (Generic)

lStateLogger :: L.Lens' LoggingActions SCL.LogAction
lStateLogger = GL.field @"sStateLogger"

lFileLogger :: L.Lens' LoggingActions (Maybe (CA.Async (), SCL.LogAction))
lFileLogger = GL.field @"sFileLogger"

-- | This is the core application state
--
-- * @e@ is the UI extension type not parameterized by the architecture.  This
--       is meant for UI state that is required even when no executable is
--       loaded.
-- * @u@ is the UI extension type, which is parameterized by the architecture.
--       This is where most of the frontend-specific state will live.
-- * @arch@ is the type of the architecture of the loaded binary.
-- * @s@ is the state thread parameter that links all uses of nonces.
data S e u (arch :: Type) s =
  S { sInputFile :: Maybe FilePath
    , sLoader :: Maybe AsyncLoader
    , sDiagnosticLevel :: !SCL.Severity
    -- ^ The level of log to display
    , sEchoArea :: !EA.EchoArea
    -- ^ An area where one-line messages can be displayed
    , sUIMode :: !(SomeUIMode s)
    -- ^ The current UI mode, which drives rendering and keybindings available
    , sAppState :: AppState
    -- ^ An indicator of the general state of the application (displayed in the
    -- status line)
    , sEventChannel :: C.Chan (SCE.Events s (S e u))
    , sLogStore :: SCL.LogStore
    -- ^ Storage for generated logs (for visualization in the UI)
    , sLogActions :: LoggingActions
    -- ^ The action to emit log messages
    , sNonceGenerator :: NG.NonceGenerator IO s
    -- ^ Nonce source used to correlate related analysis results as they stream
    -- in.  The reporting of analysis results through an existential wrapper
    -- allows us to change the type of binary being analyzed at run-time (e.g.,
    -- from 32 bit to 64 bit of a different architecture).  This flexibility
    -- comes at a cost, though, which is in complexity of determining if a
    -- streamed analysis result is of the same type as the last one.  We use
    -- nonces to track that; their 'TestEquality' instance lets us recover type
    -- equality.
    , sKeymap :: !(Keymap (AR.SurveyorCommand s (S e u)) (SomeUIMode s))
    -- ^ Keybindings mapped to commands
    , sUIExtension :: e s
    -- ^ An extension field for UI frontends for containing data that must exist
    -- even when there is no active architecture (i.e., when @arch ~ Void@).
    , sArchState :: Maybe (ArchState u arch s)
    -- ^ Architecture-specific state, including UI extensions (via the @u@
    -- parameter)
    , sArchNonce :: NG.Nonce s arch
    }
  deriving (Generic)

sEmitEvent :: forall evt e u arch s . (SCE.ToEvent evt) => S e u arch s -> evt s (S e u) -> IO ()
sEmitEvent s evt = SCE.emitEvent (sEventChannel s) evt

lNonce :: L.Lens' (S e u arch s) (NG.Nonce s arch)
lNonce = GL.field @"sArchNonce"

lLogActions :: L.Lens' (S e u arch s) LoggingActions
lLogActions = GL.field @"sLogActions"

logMessage :: S e u arch s -> SCL.LogMessage -> IO ()
logMessage s msg = do
  let actions = sLogActions s
  -- Combine the two loggers into one (if we have both) so that we can send the
  -- log message to both targets
  let logAct = maybe (sStateLogger actions) (sStateLogger actions <>) (fmap snd (sFileLogger actions))
  SCL.logMessage logAct msg

lInputFile :: L.Lens' (S e u arch s) (Maybe FilePath)
lInputFile = GL.field @"sInputFile"

lLogStore :: L.Lens' (S e u arch s) SCL.LogStore
lLogStore = GL.field @"sLogStore"

diagnosticLevelL :: L.Lens' (S e u arch s) SCL.Severity
diagnosticLevelL = GL.field @"sDiagnosticLevel"

lEchoArea :: L.Lens' (S e u arch s) EA.EchoArea
lEchoArea = GL.field @"sEchoArea"

lUIMode :: L.Lens' (S e u arch s) (SomeUIMode s)
lUIMode = GL.field @"sUIMode"

lEventChannel :: L.Lens' (S e u arch s) (C.Chan (SCE.Events s (S e u)))
lEventChannel = GL.field @"sEventChannel"

lAppState :: L.Lens' (S e u arch s) AppState
lAppState = GL.field @"sAppState"

lNonceGenerator :: L.Lens' (S e u arch s) (NG.NonceGenerator IO s)
lNonceGenerator = GL.field @"sNonceGenerator"

lLoader :: L.Lens' (S e u arch s) (Maybe AsyncLoader)
lLoader = GL.field @"sLoader"

lKeymap :: L.Lens' (S e u arch s) (Keymap (AR.SurveyorCommand s (S e u)) (SomeUIMode s))
lKeymap = GL.field @"sKeymap"

lUIExtension :: L.Lens' (S e u arch s) (e s)
lUIExtension = GL.field @"sUIExtension"

lArchState :: L.Lens' (S e u arch s) (Maybe (ArchState u arch s))
lArchState = GL.field @"sArchState"

-- | A sub-component of the state dependent on the arch type variable
--
-- This is split out so that it is easier to see these arch-dependent components
-- and replace them all at once with only one dynamic test during incremental
-- updates.
data ArchState u arch s =
  ArchState { sAnalysisResult :: !(A.AnalysisResult arch s)
            -- ^ Information returned by the binary analysis
            --
            -- We keep it around so that it doesn't have to re-index the commands
            , sContext :: !(CC.ContextStack arch s)
            -- ^ A stack of the contexts that have been focused by the user.
            --
            -- For now, we are just looking at the most recent context (i.e.,
            -- each viewer will consult the most recent context to draw).
            -- Later, there will be context-manipulation commands to allow
            -- the user to explicitly manage the stack by popping things.
            , sSymExState :: !(SE.SessionState arch s)
            -- ^ Dynamically updated state for the symbolic execution engine
            --
            -- This is referenced from the context stack, but stored separately
            -- because updates can be generated asynchronously and updating deep
            -- into the context stack structure is difficult.
            , sIRCache :: !(TC.TranslationCache arch s)
            -- ^ A cache of blocks translated from the base architecture to
            -- alternative architectures.  We need a cache, as the
            -- translation can be expensive (and we don't want to have to
            -- re-do it for each block we need to access)
            , sUIState :: !(u arch s)
            -- ^ The state required for the UI extension
            }
  deriving (Generic)

lUIState :: L.Lens' (ArchState u arch s) (u arch s)
lUIState = GL.field @"sUIState"

lAnalysisResult :: L.Lens' (ArchState u arch s) (A.AnalysisResult arch s)
lAnalysisResult = GL.field @"sAnalysisResult"

contextL :: L.Lens' (ArchState u arch s) (CC.ContextStack arch s)
contextL = GL.field @"sContext"

contextG :: L.Getter (ArchState u arch s) (CC.ContextStack arch s)
contextG = L.to (L.^. contextL)

symExStateL :: L.Lens' (ArchState u arch s) (SE.SessionState arch s)
symExStateL = GL.field @"sSymExState"

irCacheL :: L.Lens' (ArchState u arch s) (TC.TranslationCache arch s)
irCacheL = GL.field @"sIRCache"

data AppState = Loading
              | Ready
              | AwaitingFile
