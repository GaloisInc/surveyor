{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
module Surveyor.Core.State (
  State(..),
  S(..),
  ArchState(..),
  AppState(..),
  -- * Logging
  logMessage,
  logDiagnostic,
  -- * Lenses
  lInputFile,
  lLoader,
  lDiagnosticLog,
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
  irCacheL
  ) where

import           GHC.Generics ( Generic )

import qualified Control.Lens as L
import qualified Data.Generics.Product as GL
import qualified Data.Parameterized.Nonce as NG
import qualified Data.Sequence as Seq
import qualified Data.Text as T

import qualified Surveyor.Core.Chan as C
import qualified Surveyor.Core.Context as CC
import           Surveyor.Core.Keymap ( Keymap )
import qualified Surveyor.Core.Arguments as AR
import qualified Surveyor.Core.Architecture as A
import           Surveyor.Core.Events ( Events(LogDiagnostic), LogLevel )
import           Surveyor.Core.Loader ( AsyncLoader )
import           Surveyor.Core.Mode
import qualified Surveyor.Core.EchoArea as EA
import qualified Surveyor.Core.TranslationCache as TC

-- | This is a wrapper around the state type suitable for the UI core.  It hides
-- the architecture so that the architecture can change during run-time (i.e.,
-- when a new binary is loaded).
data State e u s where
  State :: (A.Architecture arch s) => !(S e u arch s) -> State e u s

-- | This is the core application state
--
-- * @e@ is the UI extension type not parameterized by the architecture.  This
--       is meant for UI state that is required even when no executable is
--       loaded.
-- * @u@ is the UI extension type, which is parameterized by the architecture.
--       This is where most of the frontend-specific state will live.
-- * @arch@ is the type of the architecture of the loaded binary.
-- * @s@ is the state thread parameter that links all uses of nonces.
data S e u (arch :: *) s =
  S { sInputFile :: Maybe FilePath
    , sLoader :: Maybe AsyncLoader
    , sDiagnosticLog :: !(Seq.Seq (Maybe LogLevel, T.Text))
    -- ^ Diagnostics collected over time (displayed in the diagnostic view)
    , sDiagnosticLevel :: !LogLevel
    , sEchoArea :: !EA.EchoArea
    -- ^ An area where one-line messages can be displayed
    , sUIMode :: !(SomeUIMode s)
    -- ^ The current UI mode, which drives rendering and keybindings available
    , sAppState :: AppState
    -- ^ An indicator of the general state of the application (displayed in the
    -- status line)
    , sEmitEvent :: Events s (S e u) -> IO ()
    -- ^ An IO action to emit an event (via the custom event channel)
    , sEventChannel :: C.Chan (Events s (S e u))
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
    -- ^ An extension field for UI frontends for containing data that is
    -- architecture-independent.  This is mostly useful for tracking the state
    -- of UI elements that have to exist even when no binary is loaded.
    , sArchState :: Maybe (ArchState u arch s)
    -- ^ Architecture-specific state, including UI extensions (via the @u@
    -- parameter)
    , sArchNonce :: NG.Nonce s arch
    }
  deriving (Generic)

lNonce :: L.Lens' (S e u arch s) (NG.Nonce s arch)
lNonce = GL.field @"sArchNonce"

logMessage :: S e u arch s -> T.Text -> IO ()
logMessage s t = sEmitEvent s (LogDiagnostic Nothing t)

logDiagnostic :: S e u arch s -> LogLevel -> T.Text -> IO ()
logDiagnostic s ll t = sEmitEvent s (LogDiagnostic (Just ll) t)

lInputFile :: L.Lens' (S e u arch s) (Maybe FilePath)
lInputFile = GL.field @"sInputFile"

lDiagnosticLog :: L.Lens' (S e u arch s) (Seq.Seq (Maybe LogLevel, T.Text))
lDiagnosticLog = GL.field @"sDiagnosticLog"

diagnosticLevelL :: L.Lens' (S e u arch s) LogLevel
diagnosticLevelL = GL.field @"sDiagnosticLevel"

lEchoArea :: L.Lens' (S e u arch s) EA.EchoArea
lEchoArea = GL.field @"sEchoArea"

lUIMode :: L.Lens' (S e u arch s) (SomeUIMode s)
lUIMode = GL.field @"sUIMode"

lEventChannel :: L.Lens' (S e u arch s) (C.Chan (Events s (S e u)))
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

irCacheL :: L.Lens' (ArchState u arch s) (TC.TranslationCache arch s)
irCacheL = GL.field @"sIRCache"

data AppState = Loading
              | Ready
              | AwaitingFile
