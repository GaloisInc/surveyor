{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
module Surveyor.State (
  State(..),
  S(..),
  ArchState(..),
  AppState(..),
  -- * Lenses
  lInputFile,
  lDiagnosticLog,
  lEchoArea,
  lUIMode,
  lAppState,
  lNonceGenerator,
  lArchState,
  lNonce,
  lAnalysisResult,
  lMinibuffer,
  lFunctionSelector,
  lBlockSelector,
  lBlockViewer,
  lKeymap
  ) where

import           GHC.Generics ( Generic )

import qualified Brick.BChan as B
import qualified Control.Lens as L
import qualified Data.Generics.Product as GL
import qualified Data.Parameterized.Nonce as NG
import qualified Data.Sequence as Seq
import qualified Data.Text as T

import           Brick.Keymap ( Keymap )
import qualified Surveyor.Arguments as AR
import qualified Surveyor.Architecture as A
import           Surveyor.Events ( Events )
import           Surveyor.Mode
import           Surveyor.Names ( Names )
import qualified Surveyor.Widget.BlockSelector as BS
import qualified Surveyor.Widget.BlockViewer as BV
import qualified Surveyor.Widget.EchoArea as EA
import qualified Surveyor.Widget.FunctionSelector as FS
import qualified Surveyor.Widget.Minibuffer as MB

data State s where
  State :: (A.Architecture arch s) => !(S arch s) -> State s

data S arch s =
  S { sInputFile :: Maybe FilePath
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
    , sArchState :: !(ArchState arch s)
    }
  deriving (Generic)

lInputFile :: L.Lens' (S arch s) (Maybe FilePath)
lInputFile = GL.field @"sInputFile"

lDiagnosticLog :: L.Lens' (S arch s) (Seq.Seq T.Text)
lDiagnosticLog = GL.field @"sDiagnosticLog"

lEchoArea :: L.Lens' (S arch s) EA.EchoArea
lEchoArea = GL.field @"sEchoArea"

lUIMode :: L.Lens' (S arch s) SomeUIMode
lUIMode = GL.field @"sUIMode"

lAppState :: L.Lens' (S arch s) AppState
lAppState = GL.field @"sAppState"

lNonceGenerator :: L.Lens' (S arch s) (NG.NonceGenerator IO s)
lNonceGenerator = GL.field @"sNonceGenerator"

lArchState :: L.Lens' (S arch s) (ArchState arch s)
lArchState = GL.field @"sArchState"

-- | A sub-component of the state dependent on the arch type variable
--
-- This is split out so that it is easier to see these arch-dependent components
-- and replace them all at once with only one dynamic test during incremental
-- updates.
data ArchState arch s =
  ArchState { sNonce :: !(NG.Nonce s arch)
            -- ^ A nonce used to check to see if the arch type has changed between runs
            , sAnalysisResult :: Maybe (A.AnalysisResult arch s)
            -- ^ Information returned by the binary analysis
            , sMinibuffer :: !(MB.Minibuffer (S arch s) (AR.Argument arch (S arch s) s) AR.TypeRepr T.Text Names)
            -- ^ The persistent state of the minibuffer
            --
            -- We keep it around so that it doesn't have to re-index the commands
            , sFunctionSelector :: !(FS.FunctionSelector arch s)
            -- ^ Functions available in the function selector
            , sBlockSelector :: !(BS.BlockSelector arch s)
            , sBlockViewer :: !(BV.BlockViewer arch s)
            , sKeymap :: !(Keymap SomeUIMode (S arch s) (AR.Argument arch (S arch s) s) AR.TypeRepr)
            }
  deriving (Generic)

lNonce :: L.Lens' (S arch s) (NG.Nonce s arch)
lNonce = lArchState . GL.field @"sNonce"

lAnalysisResult :: L.Lens' (S arch s) (Maybe (A.AnalysisResult arch s))
lAnalysisResult = lArchState . GL.field @"sAnalysisResult"

lMinibuffer :: L.Lens' (S arch s) (MB.Minibuffer (S arch s) (AR.Argument arch (S arch s) s) AR.TypeRepr T.Text Names)
lMinibuffer = lArchState . GL.field @"sMinibuffer"

lFunctionSelector :: L.Lens' (S arch s) (FS.FunctionSelector arch s)
lFunctionSelector = lArchState . GL.field @"sFunctionSelector"

lBlockSelector :: L.Lens' (S arch s) (BS.BlockSelector arch s)
lBlockSelector = lArchState . GL.field @"sBlockSelector"

lBlockViewer :: L.Lens' (S arch s) (BV.BlockViewer arch s)
lBlockViewer = lArchState . GL.field @"sBlockViewer"

lKeymap :: L.Lens' (S arch s) (Keymap SomeUIMode (S arch s) (AR.Argument arch (S arch s) s) AR.TypeRepr)
lKeymap = lArchState . GL.field @"sKeymap"

data AppState = Loading
              | Ready
              | AwaitingFile

