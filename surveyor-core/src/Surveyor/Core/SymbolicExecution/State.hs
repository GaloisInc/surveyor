{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
module Surveyor.Core.SymbolicExecution.State (
  SymbolicState(..),
  SymbolicExecutionState(..),
  SuspendedState(..),
  SuspendedReason(..),
  ExecutionProgress(..),
  RecordedStateLog(..),
  emptyMetrics,
  SymExK,
  Config,
  SetupArgs,
  Execute,
  Inspect,
  Suspend
  ) where

import           Control.DeepSeq ( NFData(..), deepseq )
import qualified Crux.Types as CT
import qualified Data.Functor.Identity as I
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.Nonce as PN
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Sequence   as Seq
import qualified Lang.Crucible.Backend as CB
import qualified Lang.Crucible.CFG.Core as CCC
import qualified Lang.Crucible.Simulator as CS
import qualified Lang.Crucible.Simulator.CallFrame as LCSC
import qualified Lang.Crucible.Simulator.ExecutionTree as CSET
import qualified Lang.Crucible.Simulator.Profiling as CSP
import qualified Lang.Crucible.Simulator.RegMap as LMCR
import qualified Surveyor.Core.Breakpoint as SCB
import qualified System.IO as IO
import qualified What4.Expr as WEB
import qualified What4.Interface as WI

import qualified Surveyor.Core.Architecture as CA
import           Surveyor.Core.SymbolicExecution.Config
import qualified Surveyor.Core.SymbolicExecution.ExecutionFeature as SCEF

data SymbolicState arch s sym init reg =
  SymbolicState { symbolicConfig :: SymbolicExecutionConfig s
                , symbolicBackend :: sym
                , someCFG :: CCC.SomeCFG (CA.CrucibleExt arch) init reg
                , symbolicGlobals :: CS.SymGlobalState sym
                , withSymConstraints :: forall a . (CB.IsSymInterface sym) => a -> a
                }

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
  Initializing :: (CB.IsSymInterface sym, sym ~ WEB.ExprBuilder s state fs)
               => SymbolicState arch s sym init reg
               -> Ctx.Assignment (CS.RegEntry sym) init
               -- ^ Initial symbolic values for starting the simulator
               -> SymbolicExecutionState arch s SetupArgs
  -- | Holds the state of the symbolic execution engine while it is executing.
  -- This includes incremental metrics and output, as well as the means to
  -- interrupt execution
  Executing :: (ext ~ CA.CrucibleExt arch) => ExecutionProgress s p sym ext -> SymbolicExecutionState arch s Execute
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
  -- | The state used when symbolic execution is suspended (but not completed),
  -- allowing various debugging commands (e.g., step or continue) to be used.
  -- It also allows for inspection of the suspended simulator state.
  Suspended :: ( CB.IsSymInterface sym
               , CA.Architecture arch s
               , ext ~ CA.CrucibleExt arch
               , sym ~ WEB.ExprBuilder s st fs
               )
            => PN.Nonce s sym
            -- ^ The nonce allowing us to prove that two symbolic backends are the same
            -> SuspendedState sym init reg p ext args blocks ret rtp f a ctx arch s
            -> SymbolicExecutionState arch s Suspend

-- | The reason that execution was suspended
data SuspendedReason p sym ext rtp where
  -- | The user set a breakpoint that was encountered (either via override or
  -- explicitly in the UI)
  SuspendedBreakpoint :: SCB.Breakpoint sym -> SuspendedReason p sym ext rtp
  -- | The execution is suspended because an assertion has failed and we have a
  -- model containing values that demonstrate the potential assertion failure
  SuspendedAssertionFailure :: CT.ModelView -> SuspendedReason p sym ext rtp
  -- | The execution is suspended because the execution feature has paused (due
  -- to single stepping)
  SuspendedExecutionStep :: CSET.ExecState p sym ext rtp -> SuspendedReason p sym ext rtp


-- | The recorded states, as well as the current index into the log (if we are stepping back through time)
data RecordedStateLog p sym ext where
  RecordedStateLog :: Int -> Seq.Seq (Some (CSET.ExecState p sym ext)) -> RecordedStateLog p sym ext

-- | The actual data for a suspended symbolic execution state
--
-- It is a separate data type because the record names are useful and would be
-- difficult to work with if inlined in the 'SymbolicExecutionState' GADT
-- constructor.
data SuspendedState sym init reg p ext args blocks ret rtp f a ctx arch s =
  SuspendedState { suspendedSymState :: SymbolicState arch s sym init reg
                 , suspendedSimState :: CSET.SimState p sym ext rtp f a
                 , suspendedCallFrame :: LCSC.CallFrame sym ext blocks ret args
                 , suspendedRegVals :: Ctx.Assignment (LMCR.RegEntry sym) ctx
                 , suspendedRegSelection :: Maybe (Some (Ctx.Index ctx))
                 , suspendedCurrentValue :: Maybe (Some (LMCR.RegEntry sym))
                 , suspendedResumeUnmodified :: IO ()
                 -- ^ Resume symbolic execution with an unmodified symbolic
                 -- execution state
                 , suspendedDebugFeatureConfig :: SCEF.DebuggerStateRef p sym ext
                 -- ^ The reference to the debug feature state; this allows the
                 -- debugger to toggle the debug feature execution mode before
                 -- it resumes execution, enabling either single stepping (or
                 -- controlled stepping) or general continuation.
                 , suspendedReason :: SuspendedReason p sym ext rtp
                 -- ^ The reason execution has been suspended
                 , suspendedHistory :: Maybe (RecordedStateLog p sym ext)
                 -- ^ The log of history that could be stepped back through,
                 -- along with the pointer to the state currently being examined.
                 }

data ExecutionProgress s p sym ext =
  ExecutionProgress { executionMetrics :: CSP.Metrics I.Identity
                    , executionOutputHandle :: IO.Handle
                    , executionConfig :: SymbolicExecutionConfig s
                    , executionInterrupt :: SCEF.DebuggerStateRef p sym ext
                    , executionResume :: IO ()
                    }

emptyMetrics :: CSP.Metrics I.Identity
emptyMetrics =
  CSP.Metrics { CSP.metricSplits = I.Identity 0
              , CSP.metricMerges = I.Identity 0
              , CSP.metricAborts = I.Identity 0
              , CSP.metricSolverStats = I.Identity WI.zeroStatistics
              , CSP.metricExtraMetrics = I.Identity mempty
              }

instance NFData (ExecutionProgress s p sym ext) where
  rnf ep =
    executionConfig ep `deepseq` ()

instance NFData (SymbolicExecutionState arch s k) where
  rnf s =
    case s of
      Configuring cfg -> cfg `deepseq` ()
      Initializing symState initRegs ->
        -- We can't really make a meaningful instance for this one
        symState `seq` initRegs `seq` ()
      Executing progress -> progress `deepseq` ()
      Inspecting metrics symState res -> metrics `seq` symState `seq` res `seq` ()
      Suspended _ symState -> symState `seq` ()
