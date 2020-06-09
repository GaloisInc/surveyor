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
import           Data.Maybe ( fromMaybe )
import           Data.Proxy ( Proxy(..) )
import qualified Data.Text as T
import qualified Data.Vector as DV
import qualified Lang.Crucible.CFG.Core as LCCC
import qualified Lang.Crucible.Simulator.CallFrame as LCSC
import qualified Lang.Crucible.Simulator.ExecutionTree as CSET
import qualified Lang.Crucible.Simulator.RegValue as LCSR
import qualified Lang.Crucible.Types as LCT
import qualified What4.Expr.Builder as WEB

import           Surveyor.Brick.Names ( Names(..) )
import qualified Surveyor.Core as C

import qualified Surveyor.Brick.Widget.ValueViewer as WVV

-- | Render a view of the final state from symbolic execution
--
-- Eventually, this should provide some mechanisms for deeply inspecting the
-- state (including arch-specific inspection of memory).
renderSymbolicExecutionStateExplorer :: forall arch s
                                      . C.SymbolicExecutionState arch s C.Suspend
                                     -> B.Widget Names
renderSymbolicExecutionStateExplorer st =
  case st of
    C.Suspended _surveyorSimState crucSimState mbp ->
      let topFrame = crucSimState ^. CSET.stateTree . CSET.actFrame
      in case topFrame ^. CSET.gpValue of
        LCSC.RF {} -> B.txt "Unexpected Return Frame"
        LCSC.OF {} -> B.txt "Unexpected Override Frame"
        LCSC.MF cf@(LCSC.CallFrame { LCSC._frameCFG = fcfg
                                   }) ->
          B.vBox [ B.txt "Current Function:" B.<+> B.txt (T.pack (show (LCCC.cfgHandle fcfg)))
                 , B.txt "Current Block:" B.<+> B.txt (T.pack (show (cf ^. LCSC.frameBlockID)))
                 , B.txt "Breakpoint name:" B.<+> B.txt (fromMaybe "<Unnamed Breakpoint>" (C.breakpointName =<< mbp))
                 , maybe B.emptyWidget (renderFirstValue Proxy) (fmap C.breakpointArguments mbp)
                 ]

-- | This is temporary: we need to provide a selector to choose the value to render
renderFirstValue :: (sym ~ WEB.ExprBuilder s st fs)
                 => proxy sym
                 -> DV.Vector (LCSR.RegValue sym LCT.AnyType)
                 -> B.Widget Names
renderFirstValue proxy vals
  | DV.length vals == 0 = B.emptyWidget
  | otherwise =
    case vals DV.! 0 of
      LCSR.AnyValue tp val ->
        let vv = WVV.valueViewer proxy tp val
        in WVV.renderValueViewer vv

handleSymbolicExecutionStateExplorerEvent :: B.BrickEvent Names e
                                          -> C.SymbolicExecutionState arch s C.Suspend
                                          -> B.EventM Names (C.SymbolicExecutionState arch s C.Suspend)
handleSymbolicExecutionStateExplorerEvent _evt st = return st
