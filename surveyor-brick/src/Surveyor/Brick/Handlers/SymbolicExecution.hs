{-# LANGUAGE GADTs #-}
-- | Brick-level handlers for symbolic execution events
module Surveyor.Brick.Handlers.SymbolicExecution ( handleSymbolicExecutionEvent ) where

import qualified Brick as B
import           Control.Lens ( (&), (^.), (%~), _Just )
import           Control.Monad.IO.Class ( liftIO )
import qualified Data.Map.Strict as Map
import qualified Data.Parameterized.Classes as PC
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Text as T
import           GHC.Stack ( HasCallStack )
import           Surveyor.Brick.Names ( Names(..) )
import qualified Surveyor.Core as C

import qualified Surveyor.Brick.Extension as SBE
import qualified Surveyor.Brick.Widget.SymbolicExecution as SEM

-- | This handler provides brick UI-specific handling for symbolic execution events
--
-- This handler is intended to run after the core-provided handlers, providing
-- hooks to update UI state when needed.
--
-- We really only need to do one thing: capture any updates to the symbolic
-- execution state so that we can rebuild the relevant UI elements
handleSymbolicExecutionEvent :: (C.Architecture arch s, HasCallStack)
                             => C.S SBE.BrickUIExtension SBE.BrickUIState arch s
                             -> C.SymbolicExecutionEvent s (C.S SBE.BrickUIExtension SBE.BrickUIState)
                             -> B.EventM Names (B.Next (C.State SBE.BrickUIExtension SBE.BrickUIState s))
handleSymbolicExecutionEvent s0 evt =
  case evt of
    C.InitializeSymbolicExecution {} -> B.continue (C.State s0)
    C.BeginSymbolicExecutionSetup {} -> B.continue (C.State s0)
    C.StartSymbolicExecution {} -> B.continue (C.State s0)
    C.ReportSymbolicExecutionMetrics {} -> B.continue (C.State s0)
    C.NameValue {} -> B.continue (C.State s0)
    C.InitializeValueNamePrompt {} -> B.continue (C.State s0)
    C.SetCurrentSymbolicExecutionValue {} -> B.continue (C.State s0)
    C.UpdateSymbolicExecutionState archNonce newState
      | Just PC.Refl <- PC.testEquality archNonce (s0 ^. C.lNonce) -> do
          -- Whenever the symbolic execution state changes, we need to rebuild
          -- the UI for the corresponding session ID
          let sessionID = C.symbolicSessionID newState

          let msg = C.msgWithContext { C.logLevel = C.Debug
                                     , C.logText = [ T.pack ("Updating widget for session " ++ show sessionID) ]
                                     }
          liftIO $ C.logMessage s0 msg

          let manager = SEM.symbolicExecutionManager (Some newState)
          let s1 = s0 & C.lArchState . _Just . C.lUIState . SBE.symbolicExecutionStateL %~ Map.insert sessionID manager
          B.continue (C.State s1)
      | otherwise -> B.continue (C.State s0)
