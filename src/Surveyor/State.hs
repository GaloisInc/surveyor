{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
module Surveyor.State (
  State(..),
  S(..),
  AppState(..)
  ) where

import qualified Brick.BChan as B
import qualified Data.Parameterized.Nonce as NG
import qualified Data.Sequence as Seq
import qualified Data.Text as T

import           Brick.Keymap ( Keymap )
import qualified Surveyor.Architecture as A
import qualified Surveyor.BlockSelector as BS
import qualified Surveyor.BlockViewer as BV
import           Surveyor.Events ( Events )
import qualified Surveyor.Minibuffer as MB
import           Surveyor.Mode
import           Surveyor.Names ( Names )
import qualified Surveyor.EchoArea as EA
import qualified Surveyor.Widget.FunctionSelector as FS

data State s where
  State :: (A.Architecture arch s) => !(S arch s) -> State s

data S arch s =
  S { sInputFile :: Maybe FilePath
    , sAnalysisResult :: Maybe (A.AnalysisResult arch s)
    -- ^ Information returned by the binary analysis
    , sDiagnosticLog :: !(Seq.Seq T.Text)
    -- ^ Diagnostics collected over time (displayed in the diagnostic view)
    , sEchoArea :: !EA.EchoArea
    -- ^ An area where one-line messages can be displayed
    , sUIMode :: !SomeUIMode
    -- ^ The current UI mode, which drives rendering and keybindings available
    , sMinibuffer :: !(MB.Minibuffer (S arch s) (MB.Argument arch (S arch s) s) MB.TypeRepr T.Text Names)
    -- ^ The persistent state of the minibuffer
    --
    -- We keep it around so that it doesn't have to re-index the commands
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
    , sFunctionSelector :: !(FS.FunctionSelector arch s)
    -- ^ Functions available in the function selector
    , sBlockSelector :: !(BS.BlockSelector arch s)
    , sBlockViewer :: !(BV.BlockViewer arch s)
    , sKeymap :: !(Keymap SomeUIMode (S arch s) (MB.Argument arch (S arch s) s) MB.TypeRepr)
    , sArch :: !(NG.Nonce s arch)
    -- ^ A nonce used to check to see if the arch type has changed between runs
    }

data AppState = Loading
              | Ready
              | AwaitingFile

