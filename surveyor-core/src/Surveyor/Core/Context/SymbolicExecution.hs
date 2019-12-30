{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
module Surveyor.Core.Context.SymbolicExecution (
  SymbolicExecutionConfig(..),
  defaultSymbolicExecutionConfig,
  Solver(..),
  SymbolicExecutionState,
  initialSymbolicExecutionState,
  -- * State Constructors
  configuringSymbolicExecution,
  -- * Cleanup
  cleanupSymbolicExecutionState,
  -- * Lenses
  configSolverL,
  configFloatReprL
  ) where

import           GHC.Generics ( Generic )

import qualified Control.Lens as L
import qualified Data.Generics.Product as GL
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.Nonce as PN
import           Data.Parameterized.Some ( Some(..) )
import qualified Lang.Crucible.Backend.Online as CBO
import qualified Lang.Crucible.CFG.Core as CCC
import qualified Lang.Crucible.Simulator as CS
import qualified What4.Expr.Builder as WEB

import qualified Surveyor.Core.Architecture as CA

-- Configuration of the symbolic backend

data Solver = CVC4 | Yices | Z3
  deriving (Show, Eq, Ord)

data SymbolicExecutionConfig =
  SymbolicExecutionConfig { symExecSolver :: Solver
                          , symExecFloatRepr :: Some WEB.FloatModeRepr
                          }
  deriving (Generic)

defaultSymbolicExecutionConfig :: SymbolicExecutionConfig
defaultSymbolicExecutionConfig =
  SymbolicExecutionConfig { symExecSolver = Yices
                          , symExecFloatRepr = Some WEB.FloatRealRepr
                          }

configSolverL :: L.Lens' SymbolicExecutionConfig Solver
configSolverL = GL.field @"symExecSolver"

configFloatReprL :: L.Lens' SymbolicExecutionConfig (Some WEB.FloatModeRepr)
configFloatReprL = GL.field @"symExecFloatRepr"

-- Setup of the symbolic execution engine (parameters and globals)

-- | Data kind for the symbolic execution state machine
data SymExK = Config | SetupArgs | Execute | Inspect

data SymbolicExecutionState arch s (k :: SymExK) where
  -- | Holds the current configuration during symbolic execution setup
  Configuring :: SymbolicExecutionConfig -> SymbolicExecutionState arch s 'Config
  -- | Holds the current set of initial register values (that can be
  -- incrementally modified via the UI/messages).  The type of the CFG fixes the
  -- shape of the initial registers.  It also contains initial values for global
  -- variables.
  Initializing :: (sym ~ CBO.OnlineBackend s solver (WEB.Flags fm))
               => SymbolicExecutionState arch s 'Config
               -> CCC.SomeCFG (CA.CrucibleExt arch) init reg
               -> WEB.FloatModeRepr fm
               -> sym
               -> Ctx.Assignment (CS.RegEntry sym) init
               -> CS.SymGlobalState sym
               -> SymbolicExecutionState arch s 'SetupArgs

initialSymbolicExecutionState :: SymbolicExecutionState arch s 'Config
initialSymbolicExecutionState = Configuring defaultSymbolicExecutionConfig

configuringSymbolicExecution :: SymbolicExecutionConfig -> SymbolicExecutionState arch s 'Config
configuringSymbolicExecution = Configuring

-- | Clean up any resources held by the provided state.
--
-- Using the state after this is unsafe.
cleanupSymbolicExecutionState :: SymbolicExecutionState arch s k -> IO ()
cleanupSymbolicExecutionState s =
  case s of
    Configuring {} -> return ()
