{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
module Surveyor.State (
  State(..),
  S(..),
--  FunctionListEntry(..),
  AppState(..),
  Names(..),
  stateFromAnalysisResult
  ) where

import qualified Brick.BChan as B
import           Data.Monoid
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Nonce as NG
import qualified Data.Sequence as Seq
import qualified Data.Text as T

import           Brick.Keymap ( Keymap )
import qualified Surveyor.Architecture as A
import qualified Surveyor.Commands as C
import           Surveyor.Events ( Events )
import qualified Surveyor.Keymap as K
import qualified Surveyor.Minibuffer as MB
import           Surveyor.Mode
import qualified Surveyor.EchoArea as EA

data State s where
  State :: (A.Architecture arch s) => S arch s -> State s

data S arch s =
  S { sInputFile :: Maybe FilePath
    , sAnalysisResult :: Maybe (A.AnalysisResult arch s)
    -- ^ Information returned by the binary analysis
    , sDiagnosticLog :: Seq.Seq T.Text
    -- ^ Diagnostics collected over time (displayed in the diagnostic view)
    , sEchoArea :: EA.EchoArea
    -- ^ An area where one-line messages can be displayed
    , sUIMode :: SomeUIMode
    -- ^ The current UI mode, which drives rendering and keybindings available
    , sMinibuffer :: MB.Minibuffer (MB.Argument arch s) MB.TypeRepr T.Text Names
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
--    , sFunctionList :: B.List Names (FunctionListEntry w)
    -- ^ Functions available in the function selector
--    , sBlockList :: (MM.MemAddr w, B.List Names (R.ConcreteBlock i w))
    , sKeymap :: Keymap SomeUIMode (MB.Argument arch s) MB.TypeRepr
    , sArch :: NG.Nonce s arch
    }

-- data FunctionListEntry w = FLE (R.ConcreteAddress w) T.Text Int

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

stateFromAnalysisResult :: (A.Architecture arch s)
                        => S arch0 s
                        -> A.AnalysisResult arch s
                        -> Seq.Seq T.Text
                        -> AppState
                        -> SomeUIMode
                        -> S arch s
stateFromAnalysisResult s0 ares newDiags state uiMode =
  S { sAnalysisResult = Just ares
--    , sFunctionList = B.list FunctionList funcList 1
    -- , sBlockList =
    --   case sBinaryInfo s0 of
    --     Nothing -> (MM.absoluteAddr 0, B.list BlockList V.empty 1)
    --     Just bar0 -> do
    --       let (nonceW0, nonceI0) = rNonces bar0
    --       let (nonceW1, nonceI1) = rNonces bar
    --       case (testEquality nonceW0 nonceW1, testEquality nonceI0 nonceI1) of
    --         (Just Refl, Just Refl) -> sBlockList s0
    --         _ -> (MM.absoluteAddr 0, B.list BlockList V.empty 1)
    , sDiagnosticLog = sDiagnosticLog s0 <> newDiags
    , sEchoArea = sEchoArea s0
    , sUIMode = uiMode
    , sInputFile = sInputFile s0
    , sMinibuffer =
      case testEquality (sArch s0) (A.archNonce ares) of
        Just Refl -> sMinibuffer s0
        Nothing -> MB.minibuffer MinibufferEditor MinibufferCompletionList "M-x" (C.allCommands (sEventChannel s0))
    , sAppState = state
    , sEmitEvent = sEmitEvent s0
    , sEventChannel = sEventChannel s0
    , sNonceGenerator = sNonceGenerator s0
    , sKeymap =
      case testEquality (sArch s0) (A.archNonce ares) of
        Just Refl -> sKeymap s0
        Nothing -> K.defaultKeymap (sEventChannel s0)
    , sArch = A.archNonce ares
    }
  -- where
  --   funcList = V.fromList [ FLE addr textName blockCount
  --                         | (addr, Some dfi) <- M.toList (R.biDiscoveryFunInfo (rBlockInfo bar))
  --                         , let textName = TE.decodeUtf8With TE.lenientDecode (MD.discoveredFunName dfi)
  --                         , let blockCount = M.size (dfi L.^. MD.parsedBlocks)
  --                         ]
