{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Surveyor.Brick.Widget.SymbolicExecution.Monitor (
  renderSymbolicExecutionMonitor,
  handleSymbolicExeuctionMonitorEvent
  ) where

import qualified Brick as B
import qualified Data.Functor.Identity as I
import qualified Data.Text as T
import qualified Lang.Crucible.Simulator.Profiling as CSP

import           Surveyor.Brick.Names ( Names(..) )
import qualified Surveyor.Core as C

renderSymbolicExecutionMonitor :: C.SymbolicExecutionState arch s C.Execute
                               -> B.Widget Names
renderSymbolicExecutionMonitor st =
  case st of
    C.Executing progress ->
      let metrics = C.executionMetrics progress
      in B.vBox [ B.txt "Splits" B.<+> B.txt (asText (CSP.metricSplits metrics))
                , B.txt "Merges" B.<+> B.txt (asText (CSP.metricMerges metrics))
                , B.txt "Aborts" B.<+> B.txt (asText (CSP.metricAborts metrics))
                , B.txt "SolverStats" B.<+> B.txt (asText (CSP.metricSolverStats metrics))
                ]

asText :: (Show a) => I.Identity a -> T.Text
asText = T.pack . show . I.runIdentity

-- | Handle events in the execution monitor widget
--
-- Currently, it has no interaction and is read-only
handleSymbolicExeuctionMonitorEvent :: B.BrickEvent Names e
                                    -> C.SymbolicExecutionState arch s C.Execute
                                    -> B.EventM Names (C.SymbolicExecutionState arch s C.Execute)
handleSymbolicExeuctionMonitorEvent _evt st = return st
