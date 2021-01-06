{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
module Crux.Debug.LLVM (
  debugLLVM
  ) where

import qualified Brick.BChan as BB
import qualified Control.Concurrent.Async as A
import qualified Control.Concurrent.MVar as MV
import           Control.Lens ( (^.), (&), (%~) )
import qualified Control.Monad.Catch as CMC
import           Control.Monad.IO.Class ( liftIO )
import qualified Data.Foldable as F
import qualified Data.IORef as IOR
import qualified Data.LLVM.BitCode as DLB
import qualified Data.Map.Strict as Map
import           Data.Maybe ( fromMaybe )
import qualified Data.Parameterized.Classes as PC
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.NatRepr as NR
import qualified Data.Parameterized.Nonce as PN
import           Data.Parameterized.Some ( Some(..) )
import           Data.String ( fromString )
import qualified Data.Text as T
import qualified System.Exit as SE
import qualified System.IO as IO
import qualified Text.LLVM as TL
import qualified What4.Expr.Builder as WEB

import qualified Crux as C
import qualified Crux.Debug.Config as CDC
import qualified Crux.LLVM.Overrides as CLO
import qualified Crux.Log as CL
import qualified Crux.Model as CM
import qualified Lang.Crucible.Backend as LCB
import qualified Lang.Crucible.CFG.Core as CCC
import qualified Lang.Crucible.FunctionHandle as CFH
import qualified Lang.Crucible.LLVM as LCL
import qualified Lang.Crucible.LLVM.Extension as CLE
import qualified Lang.Crucible.LLVM.Globals as CLG
import qualified Lang.Crucible.LLVM.Intrinsics as CLI
import qualified Lang.Crucible.LLVM.MemModel as CLM
import qualified Lang.Crucible.LLVM.Translation as CLT
import qualified Lang.Crucible.Simulator as LCS
import qualified Lang.Crucible.Simulator.ExecutionTree as LCSET
import qualified Lang.Crucible.Simulator.GlobalState as LCSG
import qualified Lang.Crucible.Simulator.Profiling as LCSP
import qualified Lang.Crucible.Types as LCT

import qualified Surveyor.Brick as SB
import qualified Surveyor.Core as SC
import qualified Crux.Debug.Interrupt as CBI
import qualified Crux.Debug.LLVM.Overrides as CDLO

data LLVMException = BitcodeParseException FilePath DLB.Error
                   | MemoryMissingFromGlobalVars
                   | forall w. UnsupportedX86BitWidth (NR.NatRepr w)
                   | MissingEntryPoint String
                   | EntryPointHasArguments

deriving instance Show LLVMException

instance CMC.Exception LLVMException

parseLLVM :: FilePath -> IO TL.Module
parseLLVM bcFilePath = do
  eres <- DLB.parseBitCodeFromFile bcFilePath
  case eres of
    Right m -> return m
    Left err -> CMC.throwM (BitcodeParseException bcFilePath err)

setupSimCtx :: ( CLO.ArchOk arch
               , LCB.IsSymInterface sym
               , CLM.HasLLVMAnn sym
               )
            => IO.Handle
            -> CFH.HandleAllocator
            -> sym
            -> LCS.GlobalVar CLM.Mem
            -> CLM.MemOptions
            -> CLT.LLVMContext arch
            -> LCS.SimContext (SC.LLVMPersonality sym) sym (CLE.LLVM arch)
setupSimCtx outHdl halloc sym memGlobal memOpts llvmCtx =
  LCS.initSimContext sym
                     CLI.llvmIntrinsicTypes
                     halloc
                     outHdl
                     (LCS.fnBindingsFromList [])
                     (LCL.llvmExtensionImpl memOpts)
                     (SC.LLVMPersonality memGlobal CM.emptyModel)
     & LCS.profilingMetrics %~ Map.union (llvmMetrics llvmCtx)

debugLLVM :: (?outputConfig :: CL.OutputConfig)
          => C.CruxOptions
          -> CDC.DebugOptions
          -> FilePath
          -> IO SE.ExitCode
debugLLVM cruxOpts dbgOpts bcFilePath = do
  debuggerHandleVar <- MV.newEmptyMVar
  res <- C.runSimulator cruxOpts (simulateLLVMWithDebug debuggerHandleVar cruxOpts dbgOpts bcFilePath)
  putStrLn "Taking cleanup action"
  cleanupAction <- MV.takeMVar debuggerHandleVar
  putStrLn "Running cleanup action"
  cleanupAction
  C.postprocessSimResult cruxOpts res

simulateLLVMWithDebug :: MV.MVar (IO ())
                      -> C.CruxOptions
                      -> CDC.DebugOptions
                      -> FilePath
                      -> C.SimulatorCallback
simulateLLVMWithDebug debuggerHandleVar cruxOpts dbgOpts bcFilePath = C.SimulatorCallback $ \sym _maybeOnline -> do
  llvmModule <- parseLLVM bcFilePath
  halloc <- CFH.newHandleAllocator

  let ?laxArith = CDC.laxArithmetic dbgOpts
  Some translation <- CLT.translateModule halloc llvmModule

  let llvmCtx = translation ^. CLT.transContext

  CLT.llvmPtrWidth llvmCtx $ \ptrW -> CLM.withPtrWidth ptrW $ do
    let ?recordLLVMAnnotation = \_ _ -> return ()
    let ?lc = llvmCtx ^. CLT.llvmTypeCtx
    let outHdl = ?outputConfig ^. CL.outputHandle
    let simCtx = setupSimCtx outHdl halloc sym (CLT.llvmMemVar llvmCtx) (CDC.memoryOptions dbgOpts) llvmCtx
    mem <- CLG.populateAllGlobals sym (CLT.globalInitMap translation) =<< CLG.initializeAllMemory sym llvmCtx llvmModule
    let globSt = LCL.llvmGlobals llvmCtx mem
    case CLT.llvmArch llvmCtx of
      CLE.X86Repr rep
        | Just PC.Refl <- PC.testEquality rep (NR.knownNat @64) -> do
          let ng = WEB.exprCounter sym
          archNonce <- PN.freshNonce ng
          -- We make this into a thunk so that it can be done lazily in the
          -- surveyor initialization (which is in a separate thread)
          let llvmCon =
                case SC.llvmAnalysisResultFromModule ng archNonce halloc llvmModule (Some translation) of
                  SC.SomeResult ares -> return ares
          customEventChan <- BB.newBChan 100
          let surveyorChan = SC.mkChan (BB.readBChan customEventChan) (BB.writeBChan customEventChan)
          -- Create a fresh symbolic execution session ID to use for all solver
          -- communication relating to this task
          sessionID <- SC.newSessionID ng
          overrideConfig <- SC.newOverrideConfig archNonce sessionID
          debuggerConfig <- SC.newDebuggerConfig archNonce sessionID

          -- This MVar is used to hold the (lazily-initialized) handle to the UI thread
          --
          -- We want to lazily instantiate the UI, as we don't want it to appear
          -- if it is never needed.
          surveyorMVar <- MV.newEmptyMVar
          -- This IO action starts the surveyor UI
          let initializeDebuggerUI = do
                surveyorThread <- A.async $ do
                  s0 <- SB.emptyArchState (Just bcFilePath) ng archNonce llvmCon surveyorChan
                  SB.surveyorWith customEventChan s0
                MV.putMVar surveyorMVar surveyorThread
                -- Ensure that exceptions thrown in surveyor are propagate to
                -- the main crux thread (so that they aren't lost)
                A.link surveyorThread

          initializeUIOnce <- MV.newMVar initializeDebuggerUI

          -- We start the monitor threads here so that we can have a reference
          -- to the debugger UI initialization variable.  The first thread that
          -- needs the UI will initialize it (emptying the MVar)
          overrideMonitorTask <- A.async (SC.overrideMonitor initializeUIOnce surveyorChan debuggerConfig overrideConfig)
          debugMonitorTask <- A.async (SC.debugMonitor initializeUIOnce surveyorChan debuggerConfig)
          A.link overrideMonitorTask
          A.link debugMonitorTask

          -- Set up an interrupt handler to allow users to interrupt crux-dbg
          -- with SIGUSR2 (and bring up the UI)
          CBI.installInterruptHandler (SC.debuggerConfigStateVar debuggerConfig)

          -- This is the action to run when the symbolic execution ends - tear
          -- down the helper threads and signal the debugger to exit the GUI.
          -- Then wait for all of the threads to complete cleanly.
          let cleanupAction = do
                putStrLn "Terminating monitors"
                SC.terminateDebugMonitor debuggerConfig
                SC.terminateOverrideMonitor overrideConfig
                putStrLn "Sending Surveyor a shutdown"
                SC.writeChan surveyorChan SC.Exit
                putStrLn "Waiting for termination"
                A.wait debugMonitorTask
                A.wait overrideMonitorTask

                -- If the UI was initialized, wait for the UI thread to complete
                maybeUIThread <- MV.tryTakeMVar surveyorMVar
                F.traverse_ A.wait maybeUIThread
                return ()

          MV.putMVar debuggerHandleVar cleanupAction

          -- We are reusing this code - we don't actually need the contents of this ref
          metricsRef <- IOR.newIORef SC.emptyMetrics
          (_initialMetrics, profilingFeature) <- SC.setupProfiling metricsRef surveyorChan sessionID

          let initSt = LCS.InitialState simCtx globSt LCS.defaultAbortHandler LCT.UnitRepr $
                LCS.runOverrideSim LCT.UnitRepr $ do
                  registerFunctions cruxOpts overrideConfig llvmModule translation
                  checkEntryPoint (fromMaybe "main" (CDC.entryPoint dbgOpts)) (CLT.cfgMap translation)
          -- FIXME: We can use this callback to collect live explanations in terms of solver state
          let handleExplanation = \_ _ -> return mempty
          let executionFeatures = [ SC.debuggerFeature debuggerConfig ng
                                  , LCS.genericToExecutionFeature profilingFeature
                                  ]
          return (C.RunnableStateWithExtensions initSt executionFeatures, handleExplanation)
        | otherwise -> CMC.throwM (UnsupportedX86BitWidth rep)

checkEntryPoint :: ( CLO.ArchOk arch
                  , CL.Logs
                  )
                => String
                -> CLT.ModuleCFGMap arch
                -> LCS.OverrideSim (SC.LLVMPersonality sym) sym (CLI.LLVM arch) r args ret ()
checkEntryPoint nm mp =
  case Map.lookup (fromString nm) mp of
    Nothing -> CMC.throwM (MissingEntryPoint nm)
    Just (_, CCC.AnyCFG anyCFG) ->
      case CCC.cfgArgTypes anyCFG of
        Ctx.Empty -> do
          liftIO $ CL.say "crux-dbg" ("Simulating from entry point " ++ show nm)
          _ <- LCS.callCFG anyCFG LCS.emptyRegMap
          return ()
        _ -> CMC.throwM EntryPointHasArguments

registerFunctions :: ( CLO.ArchOk llvmArch
                     , LCB.IsSymInterface sym
                     , CLM.HasLLVMAnn sym
                     , sym ~ WEB.ExprBuilder t st fs
                     , SC.Architecture arch t
                     , SC.SymbolicArchitecture arch t
                     , ext ~ CLI.LLVM llvmArch
                     , arch ~ SC.LLVM
                     )
                  => C.CruxOptions
                  -> SC.OverrideConfig t (SC.LLVMPersonality sym) sym arch ext
                  -> TL.Module
                  -> CLT.ModuleTranslation llvmArch
                  -> LCS.OverrideSim (SC.LLVMPersonality sym) sym (CLI.LLVM llvmArch) r args ret ()
registerFunctions cruxOpts sconf llvm_module mtrans =
  do let llvm_ctx = mtrans ^. CLT.transContext
     let ?lc = llvm_ctx ^. CLT.llvmTypeCtx

     -- register the callable override functions
     let overrides = concat [ CLO.cruxLLVMOverrides
                            , CLO.svCompOverrides
                            , CLO.cbmcOverrides
                            , CDLO.debugOverrides cruxOpts sconf
                            ]
     CLI.register_llvm_overrides llvm_module [] overrides llvm_ctx

     -- register all the functions defined in the LLVM module
     mapM_ (LCL.registerModuleFn llvm_ctx) $ Map.elems $ CLT.cfgMap mtrans

llvmMetrics :: forall arch p sym ext
             . CLT.LLVMContext arch
            -> Map.Map T.Text (LCSP.Metric p sym ext)
llvmMetrics llvmCtxt =
  Map.fromList [ ("LLVM.allocs", allocs)
               , ("LLVM.writes", writes)
               ]
  where
    allocs = LCSP.Metric $ measureMemBy CLM.memAllocCount
    writes = LCSP.Metric $ measureMemBy CLM.memWriteCount

    measureMemBy :: (CLM.MemImpl sym -> Int)
                 -> LCS.SimState p sym ext r f args
                 -> IO Integer
    measureMemBy f st = do
      let globals = st ^. LCSET.stateGlobals
      case LCSG.lookupGlobal (CLT.llvmMemVar llvmCtxt) globals of
        Just mem -> return $ toInteger (f mem)
        Nothing -> CMC.throwM MemoryMissingFromGlobalVars

