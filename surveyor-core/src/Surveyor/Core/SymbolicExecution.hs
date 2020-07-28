{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
module Surveyor.Core.SymbolicExecution (
  -- * Configuration
  SymbolicExecutionConfig(..),
  SomeFloatModeRepr(..),
  defaultSymbolicExecutionConfig,
  Solver(..),
  configSolverL,
  configFloatReprL,
  solverInteractionFileL,
  SessionID,
  sessionID,
  newSessionID,
  symbolicSessionID,
  SessionState,
  emptySessionState,
  singleSessionState,
  lookupSessionState,
  mergeSessionState,
  updateSessionMetrics,
  -- * The state of the symbolic execution automaton
  Config,
  SetupArgs,
  Execute,
  Inspect,
  Suspend,
  SymbolicExecutionState(..),
  SuspendedState(..),
  suspendedState,
  symbolicExecutionConfig,
  initialSymbolicExecutionState,
  startSymbolicExecution,
  ExecutionProgress,
  executionMetrics,
  -- * Exposed State
  SymbolicState(..),
  -- * State Constructors
  configuringSymbolicExecution,
  initializingSymbolicExecution,
  -- * Cleanup
  cleanupSymbolicExecutionState
  ) where

import           Control.DeepSeq ( NFData(..), deepseq )
import           Control.Lens ( (^.) )
import qualified Control.Lens as L
import           Control.Monad ( unless )
import qualified Control.Monad.Catch as CMC
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import qualified Data.Functor.Identity as I
import qualified Data.IORef as IOR
import qualified Data.Map.Strict as Map
import           Data.Maybe ( fromMaybe )
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.Nonce as PN
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Parameterized.TraversableFC as FC
import           Data.Proxy ( Proxy(..) )
import qualified Data.Text as T
import qualified Data.Vector as DV
import qualified Lang.Crucible.Backend as CB
import qualified Lang.Crucible.Backend.Online as CBO
import qualified Lang.Crucible.CFG.Core as CCC
import qualified Lang.Crucible.CFG.Extension as CCE
import qualified Lang.Crucible.Simulator as CS
import qualified Lang.Crucible.Simulator.CallFrame as LCSC
import qualified Lang.Crucible.Simulator.EvalStmt as CSE
import qualified Lang.Crucible.Simulator.ExecutionTree as CSET
import qualified Lang.Crucible.Simulator.Profiling as CSP
import qualified Lang.Crucible.Simulator.RegMap as LMCR
import qualified Lang.Crucible.Simulator.RegValue as LCSR
import qualified Lang.Crucible.Types as CT
import qualified Lang.Crucible.Types as LCT
import qualified Crux.Types as CT
import qualified System.IO as IO
import qualified System.Process as SP
import qualified What4.Concrete as WCC
import qualified What4.Config as WC
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
import qualified Surveyor.Core.Breakpoint as SCB
import qualified Surveyor.Core.Chan as SCC
import qualified Surveyor.Core.Events as SCE
import qualified Surveyor.Core.Panic as SCP

import           Surveyor.Core.SymbolicExecution.Config
import           Surveyor.Core.SymbolicExecution.State

-- Setup of the symbolic execution engine (parameters and globals)

-- | Data kind for the symbolic execution state machine
data SymExK = Config | SetupArgs | Execute | Inspect | Suspend

type Config = 'Config
type SetupArgs = 'SetupArgs
type Execute = 'Execute
type Inspect = 'Inspect
type Suspend = 'Suspend

data SymbolicExecutionState arch s (k :: SymExK) where
  -- | Holds the current configuration during symbolic execution setup
  --
  -- We establish the user's solver choice (and basic configuration
  -- w.r.t. floating point modes) early, as it could have an effect on the next
  -- stage.
  Configuring :: SymbolicExecutionConfig s -> SymbolicExecutionState arch s Config
  -- | Holds the current set of initial register values (that can be
  -- incrementally modified via the UI/messages).  The type of the CFG fixes the
  -- shape of the initial registers.  It also contains initial values for global
  -- variables and other memory objects.
  Initializing :: (CB.IsSymInterface sym)
               => SymbolicState arch s sym init reg
               -> SymbolicExecutionState arch s SetupArgs
  -- | Holds the state of the symbolic execution engine while it is executing.
  -- This includes incremental metrics and output, as well as the means to
  -- interrupt execution
  Executing :: ExecutionProgress s -> SymbolicExecutionState arch s Execute
  -- | The state after symbolic execution has completed.  This has enough
  -- information to start examining and querying the solver about the result
  Inspecting :: (CB.IsSymInterface sym)
             => CSP.Metrics I.Identity
             -> SymbolicState arch s sym init reg
             -> CSET.ExecResult (CA.CruciblePersonality arch sym)
                                sym
                                (CA.CrucibleExt arch)
                                (CS.RegEntry sym reg)
             -> SymbolicExecutionState arch s Inspect
  Suspended :: ( CB.IsSymInterface sym
               , CA.Architecture arch s
               , ext ~ CA.CrucibleExt arch
               , sym ~ WEB.ExprBuilder s st fs
               )
            => PN.Nonce s sym
            -> SuspendedState sym init reg p ext args blocks ret rtp f a ctx arch s
            -> SymbolicExecutionState arch s Suspend

data SuspendedState sym init reg p ext args blocks ret rtp f a ctx arch s =
  SuspendedState { suspendedSymState :: SymbolicState arch s sym init reg
                 , suspendedSimState :: CSET.SimState p sym ext rtp f a
                 , suspendedCallFrame :: LCSC.CallFrame sym ext blocks ret args
                 , suspendedBreakpoint :: Maybe (SCB.Breakpoint sym)
                 , suspendedRegVals :: Ctx.Assignment (LMCR.RegEntry sym) ctx
                 , suspendedRegSelection :: Maybe (Some (Ctx.Index ctx))
                 , suspendedCurrentValue :: Maybe (Some (LMCR.RegEntry sym))
                 , suspendedModelView :: Maybe CT.ModelView
                 }

data SymbolicExecutionException =
    UnexpectedFrame T.Text T.Text
  | NoParentFrame T.Text T.Text
  deriving (Show)

instance CMC.Exception SymbolicExecutionException

suspendedState :: forall sym arch s ext st fs m init reg p rtp f a
                . ( CB.IsSymInterface sym
                  , CA.Architecture arch s
                  , ext ~ CA.CrucibleExt arch
                  , sym ~ WEB.ExprBuilder s st fs
                  , CMC.MonadThrow m
                  , MonadIO m
                  )
               => PN.NonceGenerator IO s
               -> SymbolicState arch s sym init reg
               -> CSET.SimState p sym ext rtp f a
               -> Maybe (SCB.Breakpoint sym)
               -> Maybe (CT.ModelView)
               -> m (SymbolicExecutionState arch s Suspend)
suspendedState ng surveyorSymState crucSimState mbp mmv =
  case topFrame ^. CSET.gpValue of
    LCSC.RF {} -> CMC.throwM (UnexpectedFrame "Return" bpName)
    LCSC.OF {} ->
      withParentFrame (CSET.activeFrames (crucSimState ^. CSET.stateTree)) $ \cf -> do
        symNonce <- liftIO $ PN.freshNonce ng
        let st = SuspendedState { suspendedSymState = surveyorSymState
                                , suspendedSimState = crucSimState
                                , suspendedCallFrame = cf
                                , suspendedBreakpoint = mbp
                                , suspendedRegVals = Ctx.empty
                                , suspendedRegSelection = Nothing
                                , suspendedCurrentValue = Nothing
                                , suspendedModelView = mmv
                                }
        return (Suspended symNonce st)
    LCSC.MF cf ->
      case maybe (Some Ctx.Empty) (valuesFromVector (Proxy @sym)) (fmap SCB.breakpointArguments mbp) of
        Some Ctx.Empty -> do
          symNonce <- liftIO $ PN.freshNonce ng
          let st = SuspendedState { suspendedSymState = surveyorSymState
                                  , suspendedSimState = crucSimState
                                  , suspendedCallFrame = cf
                                  , suspendedBreakpoint = mbp
                                  , suspendedRegVals = Ctx.empty
                                  , suspendedRegSelection = Nothing
                                  , suspendedCurrentValue = Nothing
                                  , suspendedModelView = mmv
                                  }
          return (Suspended symNonce st)
        Some valAssignment@(_ Ctx.:> _) -> do
          symNonce <- liftIO $ PN.freshNonce ng
          let anIndex = Ctx.lastIndex (Ctx.size valAssignment)
          let st = SuspendedState { suspendedSymState = surveyorSymState
                                  , suspendedSimState = crucSimState
                                  , suspendedCallFrame = cf
                                  , suspendedBreakpoint = mbp
                                  , suspendedRegVals = valAssignment
                                  , suspendedRegSelection = Just (Some anIndex)
                                  , suspendedCurrentValue = Nothing
                                  , suspendedModelView = mmv
                                  }
          return (Suspended symNonce st)

  where
    bpName :: T.Text
    bpName = fromMaybe "<Unnamed Breakpoint>" (SCB.breakpointName =<< mbp)
    topFrame = crucSimState ^. CSET.stateTree . CSET.actFrame

    withParentFrame :: [CSET.SomeFrame (LCSC.SimFrame sym ext)]
                    -> (forall blocks ret args. LCSC.CallFrame sym ext blocks ret args -> m t)
                    -> m t
    withParentFrame fs k =
      case fs of
        [] -> CMC.throwM (NoParentFrame "Override" bpName)
        CSET.SomeFrame (LCSC.MF cf) : _ -> k cf
        _ : _fs -> withParentFrame _fs k

valuesFromVector :: proxy sym
                 -> DV.Vector (LCSR.RegValue sym LCT.AnyType)
                 -> Some (Ctx.Assignment (LMCR.RegEntry sym))
valuesFromVector _ v = go 0 (Some Ctx.empty)
  where
    go idx (Some ctx)
      | idx >= DV.length v = Some ctx
      | otherwise =
        case v DV.! idx of
          LCSR.AnyValue tp val ->
            go (idx + 1) (Some (Ctx.extend ctx (LMCR.RegEntry tp val)))


instance NFData (SymbolicExecutionState arch s k) where
  rnf s =
    case s of
      Configuring cfg -> cfg `deepseq` ()
      Initializing symState ->
        -- We can't really make a meaningful instance for this one
        symState `seq` ()
      Executing progress -> progress `deepseq` ()
      Inspecting metrics symState res -> metrics `seq` symState `seq` res `seq` ()
      Suspended _ symState -> symState `seq` ()

-- | A wrapper around all of the dynamically updatable symbolic execution state
--
-- We need this because updating data dynamically updated in the context stack
-- is fairly expensive, and we need to do it frequently in response to
-- asynchronous symbolic execution events.
--
-- To handle this, the context stack only tracks the 'SessionID' of each
-- symbolic execution job, while this structure holds all of the dynamically
-- updated data in a format more amenable to rapid updates.
newtype SessionState arch s =
  SessionState { unSessionState :: Map.Map (SessionID s) (Some (SymbolicExecutionState arch s)) }

instance NFData (SessionState arch s) where
  rnf (SessionState m) =
    let rnfSome :: Some (SymbolicExecutionState arch s) -> ()
        rnfSome s =
          case s of
            Some st -> st `deepseq` ()
    in fmap rnfSome m `deepseq` ()

lookupSessionState :: SessionState arch s -> SessionID s -> Maybe (Some (SymbolicExecutionState arch s))
lookupSessionState ss sid = Map.lookup sid (unSessionState ss)

singleSessionState :: SymbolicExecutionState arch s k -> SessionState arch s
singleSessionState s =
  SessionState { unSessionState = Map.singleton sid (Some s) }
  where
    sid = symbolicSessionID s

emptySessionState :: SessionState arch s
emptySessionState = SessionState { unSessionState = Map.empty }

mergeSessionState :: SessionState arch s -> SessionState arch s -> SessionState arch s
mergeSessionState (SessionState m) (SessionState m') = SessionState { unSessionState = Map.union m m'  }

-- | Updates the given session ID with additional metrics IFF that 'SessionID'
-- corresponds to a session in the 'Executing' state.
--
-- Otherwise, has no effect
updateSessionMetrics :: SessionID s
                     -> CSP.Metrics I.Identity
                     -> SessionState arch s
                     -> SessionState arch s
updateSessionMetrics sid metrics ss@(SessionState m) =
  case Map.lookup sid m of
    Just (Some (Executing progress)) ->
      let ep' = progress { executionMetrics = metrics }
      in SessionState $ Map.insert sid (Some (Executing ep')) m
    _ -> ss

data ExecutionProgress s =
  ExecutionProgress { executionMetrics :: CSP.Metrics I.Identity
                    , executionOutputHandle :: IO.Handle
                    , executionConfig :: SymbolicExecutionConfig s
                    }

instance NFData (ExecutionProgress s) where
  rnf ep =
    executionConfig ep `deepseq` ()


-- FIXME: Assign a unique nonce to each symbolic execution session so that we
-- can associate metrics with the correct session as they come out of the
-- symbolic execution engine.
--
-- Morally, we need a Map (Some (Nonce s)) (Some (SymbolicExecutionState arch
-- s)) to track the current state of each session (named by a nonce).  We need
-- this because updating the context stack with streaming updates to execution
-- state would be way too expensive.

symbolicExecutionConfig :: SymbolicExecutionState arch s k -> SymbolicExecutionConfig s
symbolicExecutionConfig s =
  case s of
    Configuring c -> c
    Initializing s' -> symbolicConfig s'
    Executing s' -> executionConfig s'
    Inspecting _ s' _ -> symbolicConfig s'
    Suspended _ s' -> symbolicConfig (suspendedSymState s')

symbolicSessionID :: SymbolicExecutionState arch s k -> SessionID s
symbolicSessionID s = symbolicExecutionConfig s L.^. sessionID

-- | Construct the default state of the symbolic execution automaton
-- (initializing with the default symbolic execution configuration)
initialSymbolicExecutionState :: PN.NonceGenerator IO s -> IO (SymbolicExecutionState arch s 'Config)
initialSymbolicExecutionState ng = Configuring <$> defaultSymbolicExecutionConfig ng

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
initializingSymbolicExecution gen symExConfig@(SymbolicExecutionConfig sid solver floatRep solverFilePath) scfg@(CCC.SomeCFG cfg) = do
  withOnlineBackend gen solver floatRep solverFilePath $ \_proxy sym -> do
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
                              , modelView = Nothing
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
startSymbolicExecution :: (CA.Architecture arch s, CB.IsSymInterface sym)
                       => SCC.Chan (SCE.Events s st)
                       -> CA.AnalysisResult arch s
                       -> SymbolicState arch s sym init reg
                       -> IO ( SymbolicExecutionState arch s Execute
                             , IO (SymbolicExecutionState arch s Inspect)
                             )
startSymbolicExecution eventChan ares st =
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

      -- This starts off with no value, but we update it with the initial
      -- metrics before the error can be observed
      --
      -- We use an IORef here so that we can collect metrics when we switch to
      -- 'Inspecting' mode, since we won't have access to the location where the
      -- real metrics are stored when symbolic execution ends.
      mref <- IOR.newIORef (error "Empty initial value for symbolic execution metrics")
      (initialMetrics, profilingFeature) <- setupProfiling mref eventChan (symbolicConfig st L.^. sessionID)
      let executionFeatures = [ CSE.genericToExecutionFeature profilingFeature
                              ]

      let startExec = do
            res <- executeCrucible executionFeatures simulatorState0
            finalMetrics <- IOR.readIORef mref
            return (Inspecting finalMetrics st res)
      let progress = ExecutionProgress { executionMetrics = initialMetrics
                                       , executionOutputHandle = readH
                                       , executionConfig = symbolicConfig st
                                       }
      return (Executing progress, startExec)

-- | Create a profiling execution feature that sends collected metrics out on
-- the event channel every 100ms
setupProfiling :: IOR.IORef (CSP.Metrics I.Identity)
               -> SCC.Chan (SCE.Events s st)
               -> SessionID s
               -> IO (CSP.Metrics I.Identity, CSE.GenericExecutionFeature sym)
setupProfiling ref chan sid = do
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
      IOR.writeIORef ref metrics
      SCE.emitEvent chan (SCE.ReportSymbolicExecutionMetrics sid metrics)

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
            _ -> SCP.panic "Allocating symbolic values" ["Unsupported symbolic value type: " ++ show rep
                                                        ]
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
    Suspended {} -> return ()

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
                  -> T.Text
                  -> (forall proxy solver sym . (sym ~ CBO.OnlineBackend s solver (WEB.Flags fm), WPO.OnlineSolver s solver, CB.IsSymInterface sym) => proxy solver -> sym -> IO a)
                  -> IO a
withOnlineBackend gen solver floatRep solverFilePath k = do
  let features = solverFeatures solver
  let interactionFilePath = T.strip solverFilePath
  case solver of
    CVC4 -> do
      st <- CBO.initialOnlineBackendState gen features
      sym <- WEB.newExprBuilder floatRep st gen
      let proxy = Proxy @(SMT2.Writer WSC.CVC4)
      WC.extendConfig WSC.cvc4Options (WI.getConfiguration sym)
      WC.extendConfig CBO.onlineBackendOptions (WI.getConfiguration sym)

      sifSetting <- WC.getOptionSetting CBO.solverInteractionFile (WI.getConfiguration sym)
      unless (T.null interactionFilePath) $ do
        _ <- WC.setOption sifSetting (WCC.ConcreteString (WUS.UnicodeLiteral interactionFilePath))
        return ()

      k proxy sym
    Yices -> do
      st <- CBO.initialOnlineBackendState gen features
      sym <- WEB.newExprBuilder floatRep st gen
      let proxy = Proxy @(WSY.Connection s)
      WC.extendConfig WSY.yicesOptions (WI.getConfiguration sym)
      WC.extendConfig CBO.onlineBackendOptions (WI.getConfiguration sym)

      sifSetting <- WC.getOptionSetting CBO.solverInteractionFile (WI.getConfiguration sym)
      unless (T.null interactionFilePath) $ do
        _ <- WC.setOption sifSetting (WCC.ConcreteString (WUS.UnicodeLiteral interactionFilePath))
        return ()

      k proxy sym
    Z3 -> do
      st <- CBO.initialOnlineBackendState gen features
      sym <- WEB.newExprBuilder floatRep st gen
      let proxy = Proxy @(SMT2.Writer WSZ.Z3)
      WC.extendConfig WSZ.z3Options (WI.getConfiguration sym)
      WC.extendConfig CBO.onlineBackendOptions (WI.getConfiguration sym)

      sifSetting <- WC.getOptionSetting CBO.solverInteractionFile (WI.getConfiguration sym)
      unless (T.null interactionFilePath) $ do
        _ <- WC.setOption sifSetting (WCC.ConcreteString (WUS.UnicodeLiteral interactionFilePath))
        return ()

      k proxy sym
