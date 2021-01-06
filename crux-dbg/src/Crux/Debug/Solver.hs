module Crux.Debug.Solver (
  withSolverAdapter,
  parseSolverOffline
  ) where

import qualified Crux as C
import qualified Crux.Config.Solver as CCS
import qualified What4.Solver as WS

-- TODO: export in crux
withSolverAdapter :: CCS.SolverOffline -> (WS.SolverAdapter st -> a) -> a
withSolverAdapter solverOff k =
  case solverOff of
    CCS.Boolector -> k WS.boolectorAdapter
    CCS.DReal -> k WS.drealAdapter
    CCS.SolverOnline CCS.CVC4 -> k WS.cvc4Adapter
    CCS.SolverOnline CCS.STP -> k WS.stpAdapter
    CCS.SolverOnline CCS.Yices -> k WS.yicesAdapter
    CCS.SolverOnline CCS.Z3 -> k WS.z3Adapter

parseSolverOffline :: C.CruxOptions -> CCS.SolverOffline
parseSolverOffline cruxOpts =
  case CCS.parseSolverConfig cruxOpts of
    Right (CCS.SingleOnlineSolver _onSolver) -> CCS.SolverOnline _onSolver
    Right (CCS.OnlineSolverWithOfflineGoals _ _offSolver) -> _offSolver
    -- FIXME: This type should really be a non-empty list
    Right (CCS.OnlyOfflineSolvers []) -> error "No solvers specified"
    Right (CCS.OnlyOfflineSolvers (offSolver:_)) -> offSolver
    Right (CCS.OnlineSolverWithSeparateOnlineGoals _ _onSolver) -> CCS.SolverOnline _onSolver
    Left _ -> CCS.SolverOnline CCS.Yices
