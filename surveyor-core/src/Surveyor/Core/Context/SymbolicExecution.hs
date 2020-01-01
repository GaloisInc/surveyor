{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
module Surveyor.Core.Context.SymbolicExecution (
  -- * Configuration
  SymbolicExecutionConfig,
  SomeFloatModeRepr(..),
  defaultSymbolicExecutionConfig,
  Solver(..),
  configSolverL,
  configFloatReprL,
  solverInteractionFileL,
  -- * The state of the symbolic execution automaton
  Config,
  SetupArgs,
  Execute,
  Inspect,
  SymbolicExecutionState(..),
  symbolicExecutionConfig,
  initialSymbolicExecutionState,
  startSymbolicExecution,
  -- * Exposed State
  SymbolicState(..),
  -- * State Constructors
  configuringSymbolicExecution,
  initializingSymbolicExecution,
  -- * Cleanup
  cleanupSymbolicExecutionState
  ) where

import qualified Control.Lens as L
import           Control.Monad ( unless )
import qualified Data.Functor.Identity as I
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.Nonce as PN
import qualified Data.Parameterized.TraversableFC as FC
import           Data.Proxy ( Proxy(..) )
import qualified Data.Text as T
import qualified Lang.Crucible.Backend as CB
import qualified Lang.Crucible.Backend.Online as CBO
import qualified Lang.Crucible.CFG.Core as CCC
import qualified Lang.Crucible.CFG.Extension as CCE
import qualified Lang.Crucible.Simulator as CS
import qualified Lang.Crucible.Simulator.EvalStmt as CSE
import qualified Lang.Crucible.Simulator.ExecutionTree as CSET
import qualified Lang.Crucible.Simulator.Profiling as CSP
import qualified Lang.Crucible.Types as CT
import qualified System.IO as IO
import qualified System.Process as SP
import qualified What4.Config as WC
import qualified What4.Concrete as WCC
import qualified What4.Expr.Builder as WEB
import qualified What4.Interface as WI
import qualified What4.InterpretedFloatingPoint as WIF
import qualified What4.ProblemFeatures as WPF
import qualified What4.Protocol.Online as WPO
import qualified What4.Protocol.SMTLib2 as SMT2
import qualified What4.Solver.CVC4 as WSC
import qualified What4.Solver.Yices as WSY
import qualified What4.Solver.Z3 as WSZ
import qualified What4.Symbol as WS
import qualified What4.Utils.StringLiteral as WUS

import qualified Surveyor.Core.Architecture as CA
import qualified Surveyor.Core.Chan as SCC
import qualified Surveyor.Core.Events as SCE

import           Surveyor.Core.Context.SymbolicExecution.Config

-- Setup of the symbolic execution engine (parameters and globals)

-- | Data kind for the symbolic execution state machine
data SymExK = Config | SetupArgs | Execute | Inspect

type Config = 'Config
type SetupArgs = 'SetupArgs
type Execute = 'Execute
type Inspect = 'Inspect

data SymbolicExecutionState arch s (k :: SymExK) where
  -- | Holds the current configuration during symbolic execution setup
  Configuring :: SymbolicExecutionConfig s -> SymbolicExecutionState arch s Config
  -- | Holds the current set of initial register values (that can be
  -- incrementally modified via the UI/messages).  The type of the CFG fixes the
  -- shape of the initial registers.  It also contains initial values for global
  -- variables.
  Initializing :: SymbolicState arch s solver fm init reg
               -> SymbolicExecutionState arch s SetupArgs
  -- | Holds the state of the symbolic execution engine while it is executing.
  -- This includes incremental metrics and output, as well as the means to
  -- interrupt execution
  Executing :: ExecutionProgress s -> SymbolicExecutionState arch s Execute
  -- | The state after symbolic execution has completed.  This has enough
  -- information to start examining and querying the solver about the result
  Inspecting :: (sym ~ CBO.OnlineBackend s solver (WEB.Flags fm))
             => SymbolicState arch s solver fm init reg
             -> CSET.ExecResult (CA.CruciblePersonality arch sym)
                                sym
                                (CA.CrucibleExt arch)
                                (CS.RegEntry sym reg)
             -> SymbolicExecutionState arch s Inspect

data ExecutionProgress s =
  ExecutionProgress { executionMetrics :: CSP.Metrics I.Identity
                    , executionOutputHandle :: IO.Handle
                    , executionConfig :: SymbolicExecutionConfig s
                    }

data SymbolicState arch s solver fm init reg =
  SymbolicState { symbolicConfig :: SymbolicExecutionConfig s
                , symbolicBackend :: CBO.OnlineBackend s solver (WEB.Flags fm)
                , someCFG :: CCC.SomeCFG (CA.CrucibleExt arch) init reg
                , symbolicRegs :: Ctx.Assignment (CS.RegEntry (CBO.OnlineBackend s solver (WEB.Flags fm))) init
                , symbolicGlobals :: CS.SymGlobalState (CBO.OnlineBackend s solver (WEB.Flags fm))
                , withSymConstraints :: forall a . ((WPO.OnlineSolver s solver, CB.IsSymInterface (CBO.OnlineBackend s solver (WEB.Flags fm))) => a) -> a
                }

symbolicExecutionConfig :: SymbolicExecutionState arch s k -> SymbolicExecutionConfig s
symbolicExecutionConfig s =
  case s of
    Configuring c -> c
    Initializing s' -> symbolicConfig s'
    Executing s' -> executionConfig s'
    Inspecting s' _ -> symbolicConfig s'

-- | Construct the default state of the symbolic execution automaton
-- (initializing with the default symbolic execution configuration)
initialSymbolicExecutionState :: SymbolicExecutionState arch s 'Config
initialSymbolicExecutionState = Configuring defaultSymbolicExecutionConfig

-- | Construct an initial symbolic execution state with a user-provided
-- configuration
configuringSymbolicExecution :: SymbolicExecutionConfig s -> SymbolicExecutionState arch s 'Config
configuringSymbolicExecution = Configuring

-- | Construct a symbolic execution state that is ready for the user to start
-- specifying initial values (or transition to the executing state)
initializingSymbolicExecution :: forall s arch init reg
                               . (CA.Architecture arch s)
                              => PN.NonceGenerator IO s
                              -> SymbolicExecutionConfig s
                              -> CCC.SomeCFG (CA.CrucibleExt arch) init reg
                              -> IO (SymbolicExecutionState arch s 'SetupArgs)
initializingSymbolicExecution gen symExConfig@(SymbolicExecutionConfig solver floatRep solverFilePath) scfg@(CCC.SomeCFG cfg) = do
  withOnlineBackend gen solver floatRep $ \_proxy sym -> do
    sifSetting <- WC.getOptionSetting CBO.solverInteractionFile (WI.getConfiguration sym)
    let interactionFilePath = T.strip solverFilePath
    unless (T.null interactionFilePath) $ do
      _ <- WC.setOption sifSetting (WCC.ConcreteString (WUS.UnicodeLiteral interactionFilePath))
      return ()
    regs <- FC.traverseFC (allocateSymbolicEntry (Proxy @(arch, s)) sym) (CCC.cfgArgTypes cfg)
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
                              , withSymConstraints = \a -> a
                              }
    return (Initializing state)

-- | Set up the rest of the initial state for the symbolic execution engine and
-- return an action that runs the symbolic execution loop.
--
-- The intent is that the caller should probably start a separate thread to run
-- that action in to avoid blocking the GUI.
--
-- The IO callback is necessary so that this code does not need to know the
-- concrete state type (and therefore avoid import cycles).  The caller should
-- provide a callback that just does an async state update.
startSymbolicExecution :: (CA.Architecture arch s)
                       => SCC.Chan (SCE.Events s st)
                       -> (SymbolicExecutionState arch s Inspect -> IO ())
                       -> CA.AnalysisResult arch s
                       -> SymbolicState arch s solver fm init reg
                       -> IO (SymbolicExecutionState arch s Execute , IO ())
startSymbolicExecution eventChan whenDone ares st = do
  case someCFG st of
    CCC.SomeCFG cfg -> withSymConstraints st $ do
      let sym = symbolicBackend st
      let retRep = CCC.cfgReturnType cfg
      (readH, writeH) <- SP.createPipe
      (intrinsicTypes, halloc, boundFuncs, extImpl, personality) <- CA.symbolicInitializers ares sym
      let ctx = CS.initSimContext sym intrinsicTypes halloc writeH boundFuncs extImpl personality
      let globals = symbolicGlobals st
      let action = CS.regValue <$> CS.callCFG cfg (CS.RegMap (symbolicRegs st))
      let econt = CS.runOverrideSim retRep action
      let simulatorState0 = CS.InitialState ctx globals CS.defaultAbortHandler retRep econt

      (initialMetrics, profilingFeature) <- setupProfiling eventChan
      let executionFeatures = [ CSE.genericToExecutionFeature profilingFeature
                              ]

      let startExec = do
            res <- executeCrucible executionFeatures simulatorState0
            whenDone (Inspecting st res)
      let progress = ExecutionProgress { executionMetrics = initialMetrics
                                       , executionOutputHandle = readH
                                       , executionConfig = symbolicConfig st
                                       }
      return (Executing progress, startExec)

-- | Create a profiling execution feature that sends collected metrics out on
-- the event channel every 100ms
setupProfiling :: SCC.Chan (SCE.Events s st)
               -> IO (CSP.Metrics I.Identity, CSE.GenericExecutionFeature sym)
setupProfiling chan = do
  profTab <- CSP.newProfilingTable
  -- Send out the profiling event every 100ms
  let profConf = CSP.ProfilingOptions { CSP.periodicProfileInterval = 0.1
                                      , CSP.periodicProfileAction = profileAction
                                      }
  f <- CSP.profilingFeature profTab (Just profConf)
  m0 <- CSP.readMetrics profTab
  return (m0, f)
  where
    profileAction table = do
      metrics <- CSP.readMetrics table
      SCC.writeChan chan (SCE.ReportSymbolicExecutionMetrics metrics)

-- | This loop is copied from Crucible, but slightly modified to allow the GUI
-- to interrupt the simulation cleanly (both to cancel and introspect)
executeCrucible :: forall p sym ext rtp
                 . ( CB.IsSymInterface sym
                   , CCE.IsSyntaxExtension ext
                   )
                => [ CS.ExecutionFeature p sym ext rtp ]
                -- ^ Execution features to install
                -> CS.ExecState p sym ext rtp
                -- ^ Execution state to begin executing
                -> IO (CS.ExecResult p sym ext rtp)
executeCrucible execFeatures exst0 = do
  let cfg = WI.getConfiguration . L.view CS.ctxSymInterface . CSET.execStateContext $ exst0
  verbOpt <- WC.getOptionSetting WC.verbosity cfg
  let loop exst =
           CSE.dispatchExecState
             (fromInteger <$> WC.getOpt verbOpt)
             exst
             return
             (\m st -> knext =<< CSE.advanceCrucibleState m st)
      knext = foldr applyExecutionFeature loop execFeatures
      applyExecutionFeature feat m = \exst ->
             CSE.runExecutionFeature feat exst >>= \case
                  CSE.ExecutionFeatureNoChange            -> m exst
                  CSE.ExecutionFeatureModifiedState exst' -> m exst'
                  CSE.ExecutionFeatureNewState exst'      -> knext exst'

  knext exst0

-- FIXME: Plumb argument names through
allocateSymbolicEntry :: (CA.Architecture arch s, CB.IsSymInterface sym)
                      => proxy (arch, s) -> sym -> CT.TypeRepr tp -> IO (CS.RegEntry sym tp)
allocateSymbolicEntry proxy sym rep =
  case CT.asBaseType rep of
    CT.AsBaseType _btr -> do
      symVal <- allocateSymbolicValue proxy sym rep
      return CS.RegEntry { CS.regType = rep
                         , CS.regValue = symVal
                         }
    -- To handle some non-base types, we'll need a typeclass method based on arch
    CT.NotBaseType -> do
      symVal <- allocateSymbolicValue proxy sym rep
      return CS.RegEntry { CS.regType = rep
                         , CS.regValue = symVal
                         }

allocateSymbolicValue :: (CA.Architecture arch s, CB.IsSymInterface sym)
                      => proxy (arch, s) -> sym -> CT.TypeRepr tp -> IO (CS.RegValue sym tp)
allocateSymbolicValue proxy sym rep =
  case CA.freshSymbolicEntry proxy sym rep of
    Just allocator -> allocator
    Nothing ->
      case CT.asBaseType rep of
        CT.AsBaseType btr -> WI.freshConstant sym name btr
        CT.NotBaseType ->
          case rep of
            CT.StructRepr reps -> FC.traverseFC (\tp -> CS.RV <$> allocateSymbolicValue proxy sym tp) reps
            _ -> error ("Unsupported symbolic value type: " ++ show rep)
  where
    name = WS.safeSymbol "argument"

-- | Clean up any resources held by the provided state.
--
-- Using the state after this is unsafe.
cleanupSymbolicExecutionState :: SymbolicExecutionState arch s k -> IO ()
cleanupSymbolicExecutionState s =
  case s of
    Configuring {} -> return ()
    Initializing {} -> return ()
    Executing {} -> return ()
    Inspecting {} -> return ()

solverFeatures :: Solver -> WPF.ProblemFeatures
solverFeatures s =
  case s of
    CVC4 -> SMT2.defaultFeatures WSC.CVC4
    Yices -> WSY.yicesDefaultFeatures
    Z3 -> SMT2.defaultFeatures WSZ.Z3

-- | Allocate an online backend using the given SMT solver
withOnlineBackend :: forall fm s a
                   . (forall st . WIF.IsInterpretedFloatExprBuilder (WEB.ExprBuilder s st (WEB.Flags fm)))
                  => PN.NonceGenerator IO s
                  -> Solver
                  -> WEB.FloatModeRepr fm
                  -> (forall proxy solver sym . (sym ~ CBO.OnlineBackend s solver (WEB.Flags fm), WPO.OnlineSolver s solver, CB.IsSymInterface sym) => proxy solver -> sym -> IO a)
                  -> IO a
withOnlineBackend gen solver floatRep k = do
  let features = solverFeatures solver
  case solver of
    CVC4 -> do
      st <- CBO.initialOnlineBackendState gen features
      sym <- WEB.newExprBuilder floatRep st gen
      let proxy = Proxy @(SMT2.Writer WSC.CVC4)
      WC.extendConfig WSC.cvc4Options (WI.getConfiguration sym)
      WC.extendConfig CBO.onlineBackendOptions (WI.getConfiguration sym)
      k proxy sym
    Yices -> do
      st <- CBO.initialOnlineBackendState gen features
      sym <- WEB.newExprBuilder floatRep st gen
      let proxy = Proxy @(WSY.Connection s)
      WC.extendConfig WSY.yicesOptions (WI.getConfiguration sym)
      WC.extendConfig CBO.onlineBackendOptions (WI.getConfiguration sym)
      k proxy sym
    Z3 -> do
      st <- CBO.initialOnlineBackendState gen features
      sym <- WEB.newExprBuilder floatRep st gen
      let proxy = Proxy @(SMT2.Writer WSZ.Z3)
      WC.extendConfig WSZ.z3Options (WI.getConfiguration sym)
      WC.extendConfig CBO.onlineBackendOptions (WI.getConfiguration sym)
      k proxy sym
