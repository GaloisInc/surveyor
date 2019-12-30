{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
module Surveyor.Core.Context.SymbolicExecution (
  SymbolicExecutionConfig(..),
  defaultSymbolicExecutionConfig,
  Solver(..),
  SymbolicExecutionState,
  newSymbolicExecutionState
  ) where

import qualified Data.Parameterized.Nonce as PN
import qualified Lang.Crucible.Backend.Online as CBO
import qualified What4.Expr.Builder as WEB

data Solver = CVC4 | Yices | Z3
  deriving (Show)

data SymbolicExecutionConfig fm =
  SymbolicExecutionConfig { symExecSolver :: Solver
                          , symExecFloatRepr :: WEB.FloatModeRepr fm
                          }

defaultSymbolicExecutionConfig :: SymbolicExecutionConfig WEB.FloatReal
defaultSymbolicExecutionConfig =
  SymbolicExecutionConfig { symExecSolver = Yices
                          , symExecFloatRepr = WEB.FloatRealRepr
                          }

data SymbolicExecutionState arch s =
  forall init ret solver fm .
  SymbolicExecutionState { symBackend :: CBO.OnlineBackend s solver (WEB.Flags fm)
                         , symConfig :: SymbolicExecutionConfig fm
                         }

newSymbolicExecutionState :: SymbolicExecutionConfig fm
                          -> PN.NonceGenerator IO s
                          -> IO (SymbolicExecutionState arch s)
newSymbolicExecutionState cfg gen = do
  let feats = undefined
  st <- CBO.initialOnlineBackendState gen feats
  -- FIXME: Make the float mode configurable
  sym <- WEB.newExprBuilder (symExecFloatRepr cfg) st gen
  return SymbolicExecutionState { symBackend = sym
                                , symConfig = cfg
                                }
