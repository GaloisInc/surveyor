{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Surveyor.Core.Context.SymbolicExecution.State (
  SymbolicState(..)
  ) where

import qualified Data.Parameterized.Context as Ctx
import qualified Lang.Crucible.Backend as CB
import qualified Lang.Crucible.Backend.Online as CBO
import qualified Lang.Crucible.CFG.Core as CCC
import qualified Lang.Crucible.Simulator as CS
import qualified What4.Expr.Builder as WEB
import qualified What4.Protocol.Online as WPO

import qualified Surveyor.Core.Architecture as CA
import           Surveyor.Core.Context.SymbolicExecution.Config

data SymbolicState arch s solver fm init reg =
  SymbolicState { symbolicConfig :: SymbolicExecutionConfig s
                , symbolicBackend :: CBO.OnlineBackend s solver (WEB.Flags fm)
                , someCFG :: CCC.SomeCFG (CA.CrucibleExt arch) init reg
                , symbolicRegs :: Ctx.Assignment (CS.RegEntry (CBO.OnlineBackend s solver (WEB.Flags fm))) init
                , symbolicGlobals :: CS.SymGlobalState (CBO.OnlineBackend s solver (WEB.Flags fm))
                , withSymConstraints :: forall a . ((WPO.OnlineSolver s solver, CB.IsSymInterface (CBO.OnlineBackend s solver (WEB.Flags fm))) => a) -> a
                }
