{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module Surveyor.State (
  State(..),
  S(..),
  FunctionListEntry(..),
  AppState(..),
  Names(..),
  stateFromAnalysisResult
  ) where

import qualified Brick.Widgets.List as B
import qualified Control.Lens as L
import qualified Data.Map as M
import           Data.Monoid
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Nonce as NG
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Data.Vector as V

import qualified Data.Macaw.Discovery as MD
import qualified Data.Macaw.Memory as MM
import qualified Renovate as R

import           Brick.Keymap ( Keymap )
import           Surveyor.BinaryAnalysisResult ( BinaryAnalysisResult(..) )
import           Surveyor.Events ( Events )
import qualified Surveyor.Minibuffer as MB
import           Surveyor.Mode
import qualified Surveyor.EchoArea as EA

data State s where
  State :: (MM.MemWidth w) => S s i a w arch -> State s

data S s i a w arch =
  S { sInputFile :: Maybe FilePath
    , sBinaryInfo :: Maybe (BinaryAnalysisResult s i a w arch)
    -- ^ Information returned by the binary analysis
    , sDiagnosticLog :: Seq.Seq T.Text
    -- ^ Diagnostics collected over time (displayed in the diagnostic view)
    , sEchoArea :: EA.EchoArea
    -- ^ An area where one-line messages can be displayed
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
    , sKeymap :: Keymap SomeUIMode MB.Argument MB.TypeRepr
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

stateFromAnalysisResult :: (MM.MemWidth w)
                        => S s i0 a0 w0 arch0
                        -> BinaryAnalysisResult s i a w arch
                        -> Seq.Seq T.Text
                        -> AppState
                        -> SomeUIMode
                        -> S s i a w arch
stateFromAnalysisResult s0 bar newDiags state uiMode =
  S { sBinaryInfo = Just bar
    , sFunctionList = B.list FunctionList funcList 1
    , sBlockList =
      case sBinaryInfo s0 of
        Nothing -> (MM.absoluteAddr 0, B.list BlockList V.empty 1)
        Just bar0 -> do
          let (nonceW0, nonceI0) = rNonces bar0
          let (nonceW1, nonceI1) = rNonces bar
          case (testEquality nonceW0 nonceW1, testEquality nonceI0 nonceI1) of
            (Just Refl, Just Refl) -> sBlockList s0
            _ -> (MM.absoluteAddr 0, B.list BlockList V.empty 1)
    , sDiagnosticLog = sDiagnosticLog s0 <> newDiags
    , sEchoArea = sEchoArea s0
    , sUIMode = uiMode
    , sInputFile = sInputFile s0
    , sMinibuffer = sMinibuffer s0
    , sAppState = state
    , sEmitEvent = sEmitEvent s0
    , sNonceGenerator = sNonceGenerator s0
    , sKeymap = sKeymap s0
    }
  where
    funcList = V.fromList [ FLE addr textName blockCount
                          | (addr, Some dfi) <- M.toList (R.biDiscoveryFunInfo (rBlockInfo bar))
                          , let textName = TE.decodeUtf8With TE.lenientDecode (MD.discoveredFunName dfi)
                          , let blockCount = M.size (dfi L.^. MD.parsedBlocks)
                          ]
