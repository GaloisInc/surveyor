{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module Surveyor.State (
  State(..),
  S(..),
  FunctionListEntry(..),
  AppState(..),
  Names(..)
  ) where

import qualified Brick.Widgets.List as B
import qualified Data.Parameterized.Nonce as NG
import qualified Data.Sequence as Seq
import qualified Data.Text as T

import qualified Data.Macaw.Memory as MM
import qualified Renovate as R

import           Surveyor.BinaryAnalysisResult ( BinaryAnalysisResult(..) )
import           Surveyor.Events ( Events )
import qualified Surveyor.Minibuffer as MB
import           Surveyor.Mode

data State s where
  State :: (MM.MemWidth w) => S s i a w arch -> State s

data S s i a w arch =
  S { sInputFile :: Maybe FilePath
    , sBinaryInfo :: Maybe (BinaryAnalysisResult s i a w arch)
    -- ^ Information returned by the binary analysis
    , sDiagnosticLog :: Seq.Seq T.Text
    -- ^ Diagnostics collected over time (displayed in the diagnostic view)
    , sUIMode :: SomeUIMode
    -- ^ The current UI mode, which drives rendering and keybindings available
    , sMinibuffer :: MB.Minibuffer MB.Argument MB.TypeRepr T.Text Names
    -- ^ The persistent state of the minibuffer
    --
    -- We keep it around so that it doesn't have to re-index the commands
    , sAppState :: AppState
    -- ^ An indicator of the general state of the application (displayed in the
    -- status line)
    , sEmitEvent :: Events s -> IO ()
    -- ^ An IO action to emit an event (via the custom event channel)
    , sNonceGenerator :: NG.NonceGenerator IO s
    -- ^ Nonce source used to correlate related analysis results as they stream
    -- in.  The reporting of analysis results through an existential wrapper
    -- allows us to change the type of binary being analyzed at run-time (e.g.,
    -- from 32 bit to 64 bit of a different architecture).  This flexibility
    -- comes at a cost, though, which is in complexity of determining if a
    -- streamed analysis result is of the same type as the last one.  We use
    -- nonces to track that; their 'TestEquality' instance lets us recover type
    -- equality.
    , sFunctionList :: B.List Names (FunctionListEntry w)
    -- ^ Functions available in the function selector
    , sBlockList :: (MM.MemAddr w, B.List Names (R.ConcreteBlock i w))
    }

data FunctionListEntry w = FLE (R.ConcreteAddress w) T.Text Int

data AppState = Loading
              | Ready
              | AwaitingFile

data Names = DiagnosticView
           | DiagnosticContent
           | FunctionList
           | BlockList
           | MinibufferEditor
           | MinibufferCompletionList
  deriving (Eq, Ord, Show)
