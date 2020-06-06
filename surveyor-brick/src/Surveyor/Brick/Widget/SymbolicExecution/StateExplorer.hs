{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Surveyor.Brick.Widget.SymbolicExecution.StateExplorer (
  renderSymbolicExecutionStateExplorer,
  handleSymbolicExecutionStateExplorerEvent
  ) where

import qualified Brick as B
import           Control.Lens ( (^.) )
import qualified Data.Text as T
import qualified Lang.Crucible.CFG.Core as LCCC
import qualified Lang.Crucible.Simulator.CallFrame as LCSC
import qualified Lang.Crucible.Simulator.ExecutionTree as CSET

import           Surveyor.Brick.Names ( Names(..) )
import qualified Surveyor.Core as C

-- | Render a view of the final state from symbolic execution
--
-- Eventually, this should provide some mechanisms for deeply inspecting the
-- state (including arch-specific inspection of memory).
renderSymbolicExecutionStateExplorer :: forall arch s
                                      . C.SymbolicExecutionState arch s C.Suspend
                                     -> B.Widget Names
renderSymbolicExecutionStateExplorer st =
  case st of
    C.Suspended _surveyorSimState crucSimState ->
      let topFrame = crucSimState ^. CSET.stateTree . CSET.actFrame
      in case topFrame ^. CSET.gpValue of
        LCSC.RF {} -> B.txt "Unexpected Return Frame"
        LCSC.OF {} -> B.txt "Unexpected Override Frame"
        LCSC.MF cf@(LCSC.CallFrame { LCSC._frameCFG = fcfg
                                   }) ->
          B.vBox [ B.txt "Current Function:" B.<+> B.txt (T.pack (show (LCCC.cfgHandle fcfg)))
                 , B.txt "Current Block:" B.<+> B.txt (T.pack (show (cf ^. LCSC.frameBlockID)))
                 ]

handleSymbolicExecutionStateExplorerEvent :: B.BrickEvent Names e
                                          -> C.SymbolicExecutionState arch s C.Suspend
                                          -> B.EventM Names (C.SymbolicExecutionState arch s C.Suspend)
handleSymbolicExecutionStateExplorerEvent _evt st = return st
