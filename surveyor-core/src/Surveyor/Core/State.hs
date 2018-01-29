{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
module Surveyor.Core.State (
  State(..),
  S(..),
  ArchState(..),
  AppState(..),
  -- * Lenses
  lInputFile,
  lLoader,
  lDiagnosticLog,
  lEchoArea,
  lUIMode,
  lAppState,
  lEventChannel,
  lNonceGenerator,
  lArchState,
  lNonce,
  lAnalysisResult,
  lKeymap,
  lUIState
  ) where

import           GHC.Generics ( Generic )

import qualified Brick.BChan as B
import qualified Control.Lens as L
import qualified Data.Generics.Product as GL
import qualified Data.Parameterized.Nonce as NG
import qualified Data.Sequence as Seq
import qualified Data.Text as T

import           Surveyor.Core.Keymap ( Keymap )
import qualified Surveyor.Arguments as AR
import qualified Surveyor.Architecture as A
import           Surveyor.Events ( Events )
import           Surveyor.Loader ( AsyncLoader )
import           Surveyor.Mode
import qualified Surveyor.Widget.EchoArea as EA

data State u s where
  State :: (A.Architecture arch s) => !(S u arch s) -> State u s

data S u arch s =
  S { sInputFile :: Maybe FilePath
    , sLoader :: Maybe AsyncLoader
    , sDiagnosticLog :: !(Seq.Seq T.Text)
    -- ^ Diagnostics collected over time (displayed in the diagnostic view)
    , sEchoArea :: !EA.EchoArea
    -- ^ An area where one-line messages can be displayed
    , sUIMode :: !SomeUIMode
    -- ^ The current UI mode, which drives rendering and keybindings available
    , sAppState :: AppState
    -- ^ An indicator of the general state of the application (displayed in the
    -- status line)
    , sEmitEvent :: Events s -> IO ()
    -- ^ An IO action to emit an event (via the custom event channel)
    , sEventChannel :: B.BChan (Events s)
    , sNonceGenerator :: NG.NonceGenerator IO s
    -- ^ Nonce source used to correlate related analysis results as they stream
    -- in.  The reporting of analysis results through an existential wrapper
    -- allows us to change the type of binary being analyzed at run-time (e.g.,
    -- from 32 bit to 64 bit of a different architecture).  This flexibility
    -- comes at a cost, though, which is in complexity of determining if a
    -- streamed analysis result is of the same type as the last one.  We use
    -- nonces to track that; their 'TestEquality' instance lets us recover type
    -- equality.
    , sArchState :: Maybe (ArchState u arch s)
    }
  deriving (Generic)

lInputFile :: L.Lens' (S u arch s) (Maybe FilePath)
lInputFile = GL.field @"sInputFile"

lDiagnosticLog :: L.Lens' (S u arch s) (Seq.Seq T.Text)
lDiagnosticLog = GL.field @"sDiagnosticLog"

lEchoArea :: L.Lens' (S u arch s) EA.EchoArea
lEchoArea = GL.field @"sEchoArea"

lUIMode :: L.Lens' (S u arch s) SomeUIMode
lUIMode = GL.field @"sUIMode"

lEventChannel :: L.Lens' (S u arch s) (B.BChan (Events s))
lEventChannel = GL.field @"sEventChannel"

lAppState :: L.Lens' (S u arch s) AppState
lAppState = GL.field @"sAppState"

lNonceGenerator :: L.Lens' (S u arch s) (NG.NonceGenerator IO s)
lNonceGenerator = GL.field @"sNonceGenerator"

lLoader :: L.Lens' (S u arch s) (Maybe AsyncLoader)
lLoader = GL.field @"sLoader"

lArchState :: L.Lens' (S u arch s) (Maybe (ArchState u arch s))
lArchState = GL.field @"sArchState"

-- | A sub-component of the state dependent on the arch type variable
--
-- This is split out so that it is easier to see these arch-dependent components
-- and replace them all at once with only one dynamic test during incremental
-- updates.
data ArchState u arch s =
  ArchState { sNonce :: !(NG.Nonce s arch)
            -- ^ A nonce used to check to see if the arch type has changed between runs
            , sAnalysisResult :: !(A.AnalysisResult arch s)
            -- ^ Information returned by the binary analysis
            --
            -- We keep it around so that it doesn't have to re-index the commands
            , sKeymap :: !(Keymap (Events s) SomeUIMode (Maybe (NG.Nonce s arch)) (AR.Argument arch (Events s) (Maybe (NG.Nonce s arch)) s) AR.TypeRepr)
            , sUIState :: !(u arch s)
            }
  deriving (Generic)

lUIState :: L.Lens' (ArchState u arch s) (u arch s)
lUIState = GL.field @"sUIState"

lNonce :: L.Lens' (ArchState u arch s) (NG.Nonce s arch)
lNonce = GL.field @"sNonce"

lAnalysisResult :: L.Lens' (ArchState u arch s) (A.AnalysisResult arch s)
lAnalysisResult = GL.field @"sAnalysisResult"

lKeymap :: L.Lens' (ArchState u arch s) (Keymap (Events s) SomeUIMode (Maybe (NG.Nonce s arch)) (AR.Argument arch (Events s) (Maybe (NG.Nonce s arch)) s) AR.TypeRepr)
lKeymap = GL.field @"sKeymap"

data AppState = Loading
              | Ready
              | AwaitingFile
