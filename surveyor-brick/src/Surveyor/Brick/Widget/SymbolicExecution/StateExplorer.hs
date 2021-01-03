{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Surveyor.Brick.Widget.SymbolicExecution.StateExplorer (
  StateExplorer,
  stateExplorer,
  renderSymbolicExecutionStateExplorer,
  handleSymbolicExecutionStateExplorerEvent
  ) where

import qualified Brick as B
import           Control.Lens ( (^.) )
import qualified Control.Lens as L
import           Control.Monad.IO.Class ( liftIO )
import           Data.Maybe ( fromMaybe )
import qualified Data.Parameterized.Classes as PC
import qualified Data.Parameterized.Nonce as PN
import           Data.Proxy ( Proxy(..) )
import qualified Data.Text as T
import qualified Lang.Crucible.Backend as CB
import qualified Lang.Crucible.CFG.Core as LCCC
import qualified Lang.Crucible.Simulator.CallFrame as LCSC
import qualified Lang.Crucible.Simulator.ExecutionTree as LCSET
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.Text as PPT
import qualified What4.Expr.Builder as WEB

import           Surveyor.Brick.Names ( Names(..) )
import qualified Surveyor.Brick.Panic as SBP
import qualified Surveyor.Core as C

import qualified Surveyor.Brick.Widget.CallStackViewer as WCSV
import qualified Surveyor.Brick.Widget.ModelViewer as WMV

-- | Holds the state of the StateExplorer widget
--
-- This includes the selected value state as well as the state for the value
-- viewer widget itself, all encapsulated in the 'WCSV.CallStackViewer'.  The
-- nonce lets us prove that we have the same symbolic backend as the
-- 'C.SymbolicExecutionState' so that we can move data between the two
data StateExplorer arch s e where
  StateExplorer :: (sym ~ WEB.ExprBuilder s st fs)
                => PN.Nonce s sym
                -> WCSV.CallStackViewer arch s sym e
                -> StateExplorer arch s e

-- | Construct the necessary explorer state from a suspended symbolic execution state
--
-- This is a little tricky because the values in the 'StateExplorer' capture and
-- existentially quantify some types that are also quantified under the
-- 'C.SymbolicExecutionState' constructor, hence some of the verbose type
-- signatures below.
--
-- At a high level, this function examines all of the values captured by a
-- breakpoint and builds viewers to inspect them.  We use 'Ctx.Assignment's here
-- to provide total indexing, which unfortunately makes the types a bit
-- complicated and involves a few wrapper types.
--
-- We construct a viewer for each value, as each viewer widget tracks its own
-- state about which constituent values are currently selected.  We also track
-- our own focus widget to determine if we should send events to the value
-- selector widget or the value viewer widget.
stateExplorer :: forall e arch s
               . C.SymbolicExecutionState arch s C.Suspend
              -> StateExplorer arch s e
stateExplorer (C.Suspended symNonce suspSt) =
  StateExplorer symNonce csViewer
  where
    csViewer = WCSV.callStackViewer (Proxy @arch) (C.suspendedReason suspSt) (C.suspendedRegVals suspSt) frames
    frames = suspSt ^. L.to C.suspendedSimState . LCSET.stateTree . L.to LCSET.activeFrames

-- | Render a view of the final state from symbolic execution
--
-- Eventually, this should provide some mechanisms for deeply inspecting the
-- state (including arch-specific inspection of memory).
renderSymbolicExecutionStateExplorer :: forall arch s e
                                      . C.SymbolicExecutionState arch s C.Suspend
                                     -> StateExplorer arch s e
                                     -> C.ValueNameMap s
                                     -> B.Widget Names
renderSymbolicExecutionStateExplorer (C.Suspended _symNonce1 suspSt) (StateExplorer _symNonce2 csv) valNames =
  case C.suspendedCallFrame suspSt of
    cf@LCSC.CallFrame { LCSC._frameCFG = fcfg } ->
      case C.suspendedReason suspSt of
        C.SuspendedBreakpoint bp ->
          B.vBox $ [ B.txt "Current Function:" B.<+> B.txt (T.pack (show (LCCC.cfgHandle fcfg)))
                   , B.txt "Current Block:" B.<+> B.txt (T.pack (show (cf ^. LCSC.frameBlockID)))
                   , B.txt "Breakpoint name:" B.<+> B.txt (fromMaybe "<Unnamed Breakpoint>" (C.breakpointName bp))
                   , WCSV.renderCallStackViewer True valNames csv
                   ]
        C.SuspendedAssertionFailure mv ->
          B.vBox $ [ B.txt "Current Function:" B.<+> B.txt (T.pack (show (LCCC.cfgHandle fcfg)))
                   , B.txt "Current Block:" B.<+> B.txt (T.pack (show (cf ^. LCSC.frameBlockID)))
                   , WCSV.renderCallStackViewer True valNames csv
                   , B.txt "Model:" B.<+> WMV.renderModelViewer mv
                   ]
        C.SuspendedExecutionStep execState ->
          B.vBox $ [ B.txt "Current Function:" B.<+> B.txt (T.pack (show (LCCC.cfgHandle fcfg)))
                   , B.txt "Current Block:" B.<+> B.txt (T.pack (show (cf ^. LCSC.frameBlockID)))
                   , B.txt "Symbolic State:" B.<+> prettyWidget (ppExecState execState)
                   , WCSV.renderCallStackViewer True valNames csv
                   ]

ppExecState :: LCSET.ExecState p sym ext rtp -> PP.Doc ann
ppExecState execState =
  case execState of
    LCSET.ResultState {} -> "ResultState (returning from function)"
    LCSET.AbortState rsn _ -> "AbortState " <> PP.parens (CB.ppAbortExecReason rsn)
    LCSET.UnwindCallState {} -> "UnwindCallState"
    LCSET.CallState {} -> "CallState"
    LCSET.TailCallState {} -> "TailCallState"
    LCSET.ReturnState {} -> "ReturnState"
    LCSET.RunningState {} -> "RunningState"
    LCSET.SymbolicBranchState {} -> "SymbolicBranchState"
    LCSET.ControlTransferState {} -> "ControlTransferState"
    LCSET.OverrideState {} -> "OverrideState"
    LCSET.BranchMergeState {} -> "BranchMergeState"
    LCSET.InitialState {} -> "InitialState"

prettyWidget :: PP.Doc ann -> B.Widget n
prettyWidget doc = B.txt (PPT.renderStrict (PP.layoutCompact doc))

-- | Handle events for the 'StateExplorer'
--
-- This handles changing focus between the two sub-widgets via tab.  Beyond
-- that, it delegates all events to the currently focused sub-widget.
handleSymbolicExecutionStateExplorerEvent :: C.S archEvt u arch s
                                          -> B.BrickEvent Names e
                                          -> C.SymbolicExecutionState arch s C.Suspend
                                          -> StateExplorer arch s e
                                          -> B.EventM Names (StateExplorer arch s e)
handleSymbolicExecutionStateExplorerEvent s0 evt (C.Suspended symNonce1 suspSt) (StateExplorer symNonce2 csv) = do
  case PC.testEquality symNonce1 symNonce2 of
    Nothing -> SBP.panic "handleSymbolicExecutionExplorerEvent" ["Mismatched solver nonce"]
    Just PC.Refl -> do
      csv' <- WCSV.handleCallStackViewerEvent evt csv
      -- Emit an event to update the underlying symbolic execution state (since
      -- we don't have easy access to it in the event handler, and don't want to
      -- make keeping everything in sync fragile)
      let suspSt' = suspSt { C.suspendedCurrentValue = WCSV.selectedValue csv' }
      let archNonce = s0 ^. C.lNonce
      let newState = C.Suspended symNonce1 suspSt'
      let sessionID = C.symbolicSessionID newState
      liftIO $ C.sEmitEvent s0 (C.SetCurrentSymbolicExecutionValue archNonce symNonce1 sessionID (WCSV.selectedValue csv'))
      return (StateExplorer symNonce2 csv')
