{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
module Surveyor.Core.Context.SymbolicExecution (
  -- * Configuration
  SymbolicExecutionConfig(..),
  SomeFloatModeRepr(..),
  defaultSymbolicExecutionConfig,
  Solver(..),
  configSolverL,
  configFloatReprL,
  solverInteractionFileL,
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

import qualified Control.Lens as L
import           Control.Monad ( unless )
import           Data.Maybe ( isJust )
import           Data.Parameterized.Classes ( testEquality )
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.Nonce as PN
import qualified Data.Parameterized.TraversableFC as FC
import           Data.Proxy ( Proxy(..) )
import qualified Data.Text as T
import qualified Lang.Crucible.Backend as CB
import qualified Lang.Crucible.Backend.Online as CBO
import qualified Lang.Crucible.CFG.Core as CCC
import qualified Lang.Crucible.Simulator as CS
import qualified Lang.Crucible.Types as CT
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

-- Configuration of the symbolic backend

data Solver = CVC4 | Yices | Z3
  deriving (Eq, Ord, Show)

data SomeFloatModeRepr s where
  SomeFloatModeRepr :: (forall st . WIF.IsInterpretedFloatExprBuilder (WEB.ExprBuilder s st (WEB.Flags fm)))
                    => WEB.FloatModeRepr fm
                    -> SomeFloatModeRepr s

deriving instance Show (SomeFloatModeRepr s)

instance Eq (SomeFloatModeRepr s) where
  SomeFloatModeRepr r1 == SomeFloatModeRepr r2 =
    isJust (testEquality r1 r2)

data SymbolicExecutionConfig s where
  SymbolicExecutionConfig :: (forall st . WIF.IsInterpretedFloatExprBuilder (WEB.ExprBuilder s st (WEB.Flags fm)))
                          => Solver
                          -> WEB.FloatModeRepr fm
                          -> T.Text
                          -> SymbolicExecutionConfig s

-- | a default configuration for the symbolic execution engine that uses Yices
-- and interprets floating point values as reals
defaultSymbolicExecutionConfig :: SymbolicExecutionConfig s
defaultSymbolicExecutionConfig =
  SymbolicExecutionConfig Yices WEB.FloatRealRepr ""

configSolverL :: L.Lens' (SymbolicExecutionConfig s) Solver
configSolverL f (SymbolicExecutionConfig solver fm fp) =
  fmap (\solver' -> SymbolicExecutionConfig solver' fm fp) (f solver)

configFloatReprL :: L.Lens' (SymbolicExecutionConfig s) (SomeFloatModeRepr s)
configFloatReprL f (SymbolicExecutionConfig solver fm fp) =
  fmap (\(SomeFloatModeRepr fm') -> SymbolicExecutionConfig solver fm' fp) (f (SomeFloatModeRepr fm))

solverInteractionFileL :: L.Lens' (SymbolicExecutionConfig s) T.Text
solverInteractionFileL f (SymbolicExecutionConfig solver fm fp) =
  fmap (\fp' -> SymbolicExecutionConfig solver fm fp') (f fp)

-- Setup of the symbolic execution engine (parameters and globals)

-- | Data kind for the symbolic execution state machine
data SymExK = Config | SetupArgs | Execute | Inspect

type Config = 'Config
type SetupArgs = 'SetupArgs

data SymbolicExecutionState arch s (k :: SymExK) where
  -- | Holds the current configuration during symbolic execution setup
  Configuring :: SymbolicExecutionConfig s -> SymbolicExecutionState arch s Config
  -- | Holds the current set of initial register values (that can be
  -- incrementally modified via the UI/messages).  The type of the CFG fixes the
  -- shape of the initial registers.  It also contains initial values for global
  -- variables.
  Initializing :: SymbolicState arch s solver fm init reg
               -> SymbolicExecutionState arch s SetupArgs

data SymbolicState arch s solver fm init reg =
  SymbolicState { symbolicConfig :: SymbolicExecutionConfig s
                , symbolicBackend :: CBO.OnlineBackend s solver (WEB.Flags fm)
                , someCFG :: CCC.SomeCFG (CA.CrucibleExt arch) init reg
                , symbolicRegs :: Ctx.Assignment (CS.RegEntry (CBO.OnlineBackend s solver (WEB.Flags fm))) init
                , symbolicGlobals :: CS.SymGlobalState (CBO.OnlineBackend s solver (WEB.Flags fm))
                }

symbolicExecutionConfig :: SymbolicExecutionState arch s k -> SymbolicExecutionConfig s
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
                              }
    return (Initializing state)

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
