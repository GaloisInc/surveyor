{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Surveyor.Core.State (
  State(..),
  LoggingActions(..),
  S(..),
  ArchState(..),
  AppState(..),
  sEmitEvent,
  ArchDict(..),
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
  lValueNames,
  lAnalysisResult,
  lKeymap,
  lUIState,
  contextL,
  contextG,
  symExStateL,
  archDictsL,
  archDictsG,
  irCacheL
  ) where

import           GHC.Generics ( Generic )

import qualified Control.Concurrent.Async as CA
import qualified Control.Lens as L
import           Data.Kind ( Type )
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.Nonce as NG

import qualified Surveyor.Core.Architecture as A
import qualified Surveyor.Core.Arguments as AR
import qualified Surveyor.Core.Chan as C
import qualified Surveyor.Core.Context as CC
import qualified Surveyor.Core.EchoArea as EA
import qualified Surveyor.Core.Events as SCE
import qualified Surveyor.Core.IRRepr as IR
import           Surveyor.Core.Keymap ( Keymap )
import           Surveyor.Core.Loader ( AsyncLoader )
import qualified Surveyor.Core.Logging as SCL
import           Surveyor.Core.Mode
import qualified Surveyor.Core.SymbolicExecution as SE
import qualified Surveyor.Core.TranslationCache as TC
import qualified Surveyor.Core.ValueNames as SCV

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

L.makeLensesFor
  [ ("sStateLogger", "lStateLogger")
  , ("sFileLogger", "lFileLogger") ]
  ''LoggingActions

data AppState = Loading
              | Ready
              | AwaitingFile

-- | A data type to capture some dictionaries for architectures and IRs
--
-- Pattern match on it to bring the captured dictionaries into scope.
data ArchDict arch s ir where
  ArchDict :: (A.Architecture arch s, A.IR ir s) => ArchDict arch s ir

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
            , sArchDicts :: !(MapF.MapF (IR.IRRepr arch) (ArchDict arch s))
            -- ^ Captured dictionaries for each IR; given an 'IR.IRRepr', this
            -- allows the client code to recover some class constraints.
            --
            -- It would be nice to just capture these in each 'IR.IRRepr', but
            -- that would introduce some circular dependencies unless everything
            -- was defined in the same file.
            , sUIState :: !(u arch s)
            -- ^ The state required for the UI extension
            }
  deriving (Generic)

L.makeLensesFor
  [ ("sUIState", "lUIState")
  , ("sAnalysisResult", "lAnalysisResult")
  , ("sContext", "contextL")
  , ("sArchDicts", "archDictsL")
  , ("sSymExState", "symExStateL")
  , ("sIRCache", "irCacheL") ]
  ''ArchState

contextG :: L.Getter (ArchState u arch s) (CC.ContextStack arch s)
contextG = L.to (L.^. contextL)

archDictsG :: L.Getter (ArchState u arch s) (MapF.MapF (IR.IRRepr arch) (ArchDict arch s))
archDictsG = L.to (L.^. archDictsL)

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
    , sValueNames :: !(SCV.ValueNameMap s)
    -- ^ Saved names for symbolic value (both automatically generated and user-provided)
    , sUIExtension :: e s
    -- ^ An extension field for UI frontends for containing data that must exist
    -- even when there is no active architecture (i.e., when @arch ~ Void@).
    , sArchState :: Maybe (ArchState u arch s)
    -- ^ Architecture-specific state, including UI extensions (via the @u@
    -- parameter)
    , sArchNonce :: NG.Nonce s arch
    }
  deriving (Generic)

L.makeLensesFor
  [ ("sArchNonce", "lNonce")
  , ("sLogActions", "lLogActions")
  , ("sInputFile", "lInputFile")
  , ("sLogStore", "lLogStore")
  , ("sDiagnosticLevel", "diagnosticLevelL")
  , ("sEchoArea", "lEchoArea")
  , ("sUIMode", "lUIMode")
  , ("sEventChannel", "lEventChannel")
  , ("sAppState", "lAppState")
  , ("sNonceGenerator", "lNonceGenerator")
  , ("sLoader", "lLoader")
  , ("sKeymap", "lKeymap")
  , ("sValueNames", "lValueNames")
  , ("sUIExtension", "lUIExtension")
  , ("sArchState", "lArchState") ]
  ''S

instance AR.HasNonce (S e u) where
  getNonce (AR.SomeState s) = AR.SomeNonce (sArchNonce s)

sEmitEvent :: forall evt e u arch s . (SCE.ToEvent s (S e u) evt) => S e u arch s -> evt s (S e u) -> IO ()
sEmitEvent s evt = SCE.emitEvent (sEventChannel s) evt

logMessage :: S e u arch s -> SCL.LogMessage -> IO ()
logMessage s msg = do
  let actions = sLogActions s
  -- Combine the two loggers into one (if we have both) so that we can send the
  -- log message to both targets
  let logAct = maybe (sStateLogger actions) (sStateLogger actions <>) (fmap snd (sFileLogger actions))
  SCL.logMessage logAct msg

-- | This is a wrapper around the state type suitable for the UI core.  It hides
-- the architecture so that the architecture can change during run-time (i.e.,
-- when a new binary is loaded).
data State e u s where
  State :: (A.Architecture arch s, A.CrucibleExtension arch) => !(S e u arch s) -> State e u s
