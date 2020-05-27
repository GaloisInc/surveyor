{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Surveyor.Brick.Widget.SymbolicExecution.Inspect (
  renderSymbolicExecutionInspector,
  handleSymbolicExecutionInspectorEvent
  ) where

import qualified Brick as B
import           Control.Lens ( (^.) )
import           Data.Proxy ( Proxy(..) )
import qualified Data.Text as T
import qualified Lang.Crucible.Backend.Online as CBO
import qualified Lang.Crucible.Simulator as CS
import qualified Lang.Crucible.Simulator.ExecutionTree as CSET
import qualified Lang.Crucible.Simulator.RegMap as MCR
import qualified What4.Expr as WEB
import qualified What4.Interface as WI

import           Surveyor.Brick.Names ( Names(..) )
import qualified Surveyor.Core as C
import qualified Surveyor.Brick.Widget.SymbolicExecution.Monitor as SEM

-- | Render a view of the final state from symbolic execution
--
-- Eventually, this should provide some mechanisms for deeply inspecting the
-- state (including arch-specific inspection of memory).
renderSymbolicExecutionInspector :: forall arch s
                                  . C.SymbolicExecutionState arch s C.Inspect
                                 -> B.Widget Names
renderSymbolicExecutionInspector st =
  case st of
    C.Inspecting progress symState execResult ->
      B.vBox [ B.txt "Metrics"
             , SEM.renderExecutionProgress progress
             , B.txt "Result"
             , renderExecResult (Proxy @arch) execResult
             ]

renderExecResult :: (sym ~ CBO.OnlineBackend s solver (WEB.Flags fm))
                 => proxy arch
                 -> CSET.ExecResult (C.CruciblePersonality arch sym) sym (C.CrucibleExt arch) (CS.RegEntry sym reg)
                 -> B.Widget Names
renderExecResult _ eres =
  case eres of
    CSET.TimeoutResult {} -> B.txt "Timeout"
    CSET.FinishedResult _simCtx pres ->
      case pres of
        CSET.TotalRes gp ->
          let str = MCR.asSymExpr (gp ^. CSET.gpValue) (T.pack . show . WI.printSymExpr) "Non-base type"
          in B.txt str
        CSET.PartialRes {} -> B.txt "PartialRes"
    CSET.AbortedResult {} -> B.txt "AbortedResult"

handleSymbolicExecutionInspectorEvent :: B.BrickEvent Names e
                                      -> C.SymbolicExecutionState arch s C.Inspect
                                      -> B.EventM Names (C.SymbolicExecutionState arch s C.Inspect)
handleSymbolicExecutionInspectorEvent _evt st = return st
