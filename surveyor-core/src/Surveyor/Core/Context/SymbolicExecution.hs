{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module Surveyor.Core.Context.SymbolicExecution (
  -- * Configuration
  SymbolicExecutionConfig(..),
  defaultSymbolicExecutionConfig,
  Solver(..),
  configSolverL,
  configFloatReprL,
  -- * The state of the symbolic execution automaton
  Config,
  SetupArgs,
  SymbolicExecutionState(..),
  symbolicExecutionConfig,
  initialSymbolicExecutionState,
  -- * Exposed State
  SymbolicState(..),
  -- * State Constructors
  configuringSymbolicExecution,
  initializingSymbolicExecution,
  -- * Cleanup
  cleanupSymbolicExecutionState
  ) where

import           GHC.Generics ( Generic )

import           Control.Lens ( (^.) )
import qualified Control.Lens as L
import qualified Data.Generics.Product as GL
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.Nonce as PN
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Parameterized.TraversableFC as FC
import qualified Lang.Crucible.Backend.Online as CBO
import qualified Lang.Crucible.CFG.Core as CCC
import qualified Lang.Crucible.Simulator as CS
import qualified Lang.Crucible.Types as CT
import qualified What4.Expr.Builder as WEB
import qualified What4.Interface as WI
import qualified What4.ProblemFeatures as WPF
import qualified What4.Protocol.SMTLib2 as SMT2
import qualified What4.Solver.CVC4 as WSC
import qualified What4.Solver.Yices as WSY
import qualified What4.Solver.Z3 as WSZ
import qualified What4.Symbol as WS

import qualified Surveyor.Core.Architecture as CA

-- Configuration of the symbolic backend

data Solver = CVC4 | Yices | Z3
  deriving (Show, Eq, Ord)

data SymbolicExecutionConfig =
  SymbolicExecutionConfig { symExecSolver :: Solver
                          , symExecFloatRepr :: Some WEB.FloatModeRepr
                          }
  deriving (Generic)

-- | A default configuration for the symbolic execution engine that uses Yices
-- and interprets floating point values as reals
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

type Config = 'Config
type SetupArgs = 'SetupArgs

data SymbolicExecutionState arch s (k :: SymExK) where
  -- | Holds the current configuration during symbolic execution setup
  Configuring :: SymbolicExecutionConfig -> SymbolicExecutionState arch s Config
  -- | Holds the current set of initial register values (that can be
  -- incrementally modified via the UI/messages).  The type of the CFG fixes the
  -- shape of the initial registers.  It also contains initial values for global
  -- variables.
  Initializing :: SymbolicState arch s solver fm init reg
               -> SymbolicExecutionState arch s SetupArgs

data SymbolicState arch s solver fm init reg =
  SymbolicState { symbolicConfig :: SymbolicExecutionConfig
                , symbolicBackend :: CBO.OnlineBackend s solver (WEB.Flags fm)
                , someCFG :: CCC.SomeCFG (CA.CrucibleExt arch) init reg
                , symbolicRegs :: Ctx.Assignment (CS.RegEntry (CBO.OnlineBackend s solver (WEB.Flags fm))) init
                , symbolicGlobals :: CS.SymGlobalState (CBO.OnlineBackend s solver (WEB.Flags fm))
                }

symbolicExecutionConfig :: SymbolicExecutionState arch s k -> SymbolicExecutionConfig
symbolicExecutionConfig s =
  case s of
    Configuring c -> c
    Initializing s' -> symbolicConfig s'

-- | Construct the default state of the symbolic execution automaton
-- (initializing with the default symbolic execution configuration)
initialSymbolicExecutionState :: SymbolicExecutionState arch s 'Config
initialSymbolicExecutionState = Configuring defaultSymbolicExecutionConfig

-- | Construct an initial symbolic execution state with a user-provided
-- configuration
configuringSymbolicExecution :: SymbolicExecutionConfig -> SymbolicExecutionState arch s 'Config
configuringSymbolicExecution = Configuring

-- | Construct a symbolic execution state that is ready for the user to start
-- specifying initial values (or transition to the executing state)
initializingSymbolicExecution :: PN.NonceGenerator IO s
                              -> SymbolicExecutionConfig
                              -> CCC.SomeCFG (CA.CrucibleExt arch) init reg
                              -> IO (SymbolicExecutionState arch s 'SetupArgs)
initializingSymbolicExecution gen symExConfig scfg@(CCC.SomeCFG cfg) = do
  let prevState = configuringSymbolicExecution symExConfig
  let features = solverFeatures (symExConfig ^. configSolverL)
  st <- CBO.initialOnlineBackendState gen features
  case symExConfig ^. configFloatReprL of
    Some floatRep -> do
      sym <- WEB.newExprBuilder floatRep st gen
      regs <- FC.traverseFC (allocateSymbolicValue sym) (CCC.cfgArgTypes cfg)
      -- FIXME: We don't really have a good way to enumerate all of the
      -- possibly-needed globals in a CFG... for some backends we can do it...
      --
      -- For LLVM we can use the normal setup code.  For machine code we can
      -- also set things up that way.  Maybe we should just have an
      -- arch-specific method to make a fresh set of globals.
      let globals = CS.emptyGlobals
      let state = SymbolicState { symbolicConfig = symExConfig
                                , symbolicBackend = sym
                                , someCFG = scfg
                                , symbolicRegs = regs
                                , symbolicGlobals = globals
                                }
      return (Initializing state)

-- FIXME: Plumb argument names through
allocateSymbolicValue :: (WI.IsSymExprBuilder sym) => sym -> CT.TypeRepr tp -> IO (CS.RegEntry sym tp)
allocateSymbolicValue sym rep =
  case CT.asBaseType rep of
    CT.AsBaseType btr -> do
      symVal <- WI.freshConstant sym name btr
      return CS.RegEntry { CS.regType = rep
                         , CS.regValue = symVal
                         }
    -- To handle some non-base types, we'll need a typeclass method based on arch
    CT.NotBaseType -> error "Non-base types not handled yet"
  where
    name = WS.safeSymbol "argument"

-- | Clean up any resources held by the provided state.
--
-- Using the state after this is unsafe.
cleanupSymbolicExecutionState :: SymbolicExecutionState arch s k -> IO ()
cleanupSymbolicExecutionState s =
  case s of
    Configuring {} -> return ()

solverFeatures :: Solver -> WPF.ProblemFeatures
solverFeatures s =
  case s of
    CVC4 -> SMT2.defaultFeatures WSC.CVC4
    Yices -> WSY.yicesDefaultFeatures
    Z3 -> SMT2.defaultFeatures WSZ.Z3
