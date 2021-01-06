{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Crux.Debug.LLVM.Overrides (
  debugOverrides,
  do_breakpoint,
  do_debug_assert
  ) where

import qualified Control.Concurrent.Chan as CCC
import           Control.Lens ( (^.) )
import           Control.Monad.IO.Class ( liftIO )
import qualified Control.Monad.State as CMS
import qualified Data.BitVector.Sized as BV
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Parameterized.Classes as PC
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.Nonce as PN
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import           GHC.TypeLits ( type (<=) )
import qualified What4.Expr.Builder as WEB
import qualified What4.Interface as WI
import qualified What4.ProgramLoc as WPL
import           What4.SatResult(SatResult(..))
import qualified What4.Solver as WS

import qualified Crux as C
import qualified Crux.Config.Solver as CCS
import qualified Crux.LLVM.Overrides as CLO
import qualified Crux.Model as CM
import qualified Crux.Types as C
import qualified Crux.Types as CT
import qualified Lang.Crucible.Backend as LCB
import qualified Lang.Crucible.LLVM.Extension as CLE
import qualified Lang.Crucible.LLVM.Intrinsics as CLI
import qualified Lang.Crucible.LLVM.MemModel as CLM
import qualified Lang.Crucible.LLVM.QQ as LCLQ
import qualified Lang.Crucible.LLVM.Translation as CLT
import qualified Lang.Crucible.Simulator as LCS
import qualified Lang.Crucible.Simulator.CallFrame as LCSC
import qualified Lang.Crucible.Simulator.ExecutionTree as LCSET
import qualified Lang.Crucible.Simulator.GlobalState as LCSG
import           Lang.Crucible.Simulator.RegMap (regValue)
import qualified Lang.Crucible.Types as LCT

import qualified Crux.Debug.Solver as CDS
import qualified Surveyor.Core as SC

-- | Inform the waiting debugger (via the message channel) that the symbolic
-- execution engine has stopped in an override.
--
-- This function waits for a response before resuming execution.  The response
-- could in theory modify the simulator state.
enterDebugger :: SC.OverrideConfig s p sym arch' ext
              -> PN.NonceGenerator IO s
              -> LCSET.SimState p sym ext rtp (LCSC.OverrideLang ret) ('Just args)
              -> SC.SuspendedReason p sym ext rtp
              -> LCS.OverrideSim p sym ext rtp args ret ()
enterDebugger (SC.OverrideConfig _archNonce _sessionID toDebugger fromDebugger) ng simState breakReason = do
  rtpNonce <- liftIO $ PN.freshNonce ng
  argsNonce <- liftIO $ PN.freshNonce ng
  liftIO $ CCC.writeChan toDebugger (Just (SC.CrucibleSimState rtpNonce argsNonce simState breakReason))
  response <- liftIO $ CCC.readChan fromDebugger
  case response of
    SC.UnmodifiedSimState -> return ()
    SC.ModifiedSimState rtpNonce' argsNonce' simState'
      | Just PC.Refl <- PC.testEquality rtpNonce rtpNonce'
      , Just PC.Refl <- PC.testEquality argsNonce argsNonce' -> do
          CMS.put simState'
          return ()
      | otherwise -> error "Override channel out of sync"

-- TODO: export in crux
lookupString :: (LCB.IsSymInterface sym, CLM.HasLLVMAnn sym, CLO.ArchOk arch)
             => LCS.GlobalVar CLM.Mem
             -> LCS.RegEntry sym (CLT.LLVMPointerType (CLE.ArchWidth arch))
             -> C.OverM personality sym (CLI.LLVM arch) String
lookupString mvar ptr =
  do sym <- LCS.getSymInterface
     mem <- LCS.readGlobal mvar
     bytes <- liftIO (CLM.loadString sym mem (regValue ptr) Nothing)
     return (BS8.unpack (BS.pack bytes))

-- | Halt execution when the symbolic simulator executes this override, dropping
-- into the debugger
do_breakpoint :: ( wptr ~ CLE.ArchWidth arch
                 , ext ~ CLI.LLVM arch
                 , sym ~ WEB.ExprBuilder t st fs
                 , SC.SymbolicArchitecture arch' t
                 , LCB.IsSymInterface sym
                 , 1 <= wptr
                 , 16 <= wptr
                 )
              => SC.OverrideConfig t (SC.LLVMPersonality sym) sym arch' ext
              -> LCS.GlobalVar CLM.Mem
              -> sym
              -> Ctx.Assignment (LCS.RegEntry sym) (Ctx.EmptyCtx Ctx.::> CLT.LLVMPointerType wptr Ctx.::> LCT.VectorType LCT.AnyType)
              -> LCS.OverrideSim (SC.LLVMPersonality sym) sym (CLI.LLVM arch) r args ret ()
do_breakpoint conf memVar sym (Ctx.Empty Ctx.:> breakpointNamePtr Ctx.:> breakpointValues) = do
  let ?ptrWidth = CLM.ptrWidth (LCS.regValue breakpointNamePtr)
  let ?recordLLVMAnnotation = \_ _ -> return ()
  let ng = WEB.exprCounter sym
  simState <- CMS.get
  bpName <- case LCSG.lookupGlobal memVar (simState ^. LCSET.stateGlobals) of
              Nothing -> return "<Unnamed Breakpoint>"
              Just memImpl -> do
                chars <- liftIO $ CLM.loadString sym memImpl (LCS.regValue breakpointNamePtr) Nothing
                return (TE.decodeUtf8With TE.lenientDecode (BS.pack chars))
  let bp = SC.Breakpoint { SC.breakpointType = SC.UnconditionalBreakpoint
                         , SC.breakpointName = Just bpName
                         , SC.breakpointArguments = LCS.regValue breakpointValues
                         }
  enterDebugger conf ng simState (SC.SuspendedBreakpoint bp)

data WantDebug sym = WantDebug CT.ModelView | NoDebug

-- | Like the standard assertion primitive from crux, except that it checks the
-- validity of each assertion when it is encountered.  If the assertion is not
-- valid in this context, the override drops execution into the debugger.
--
-- The goal is to localize errors for easier diagnosis.
do_debug_assert :: ( CLO.ArchOk arch
                  , LCB.IsSymInterface sym
                  , CLM.HasLLVMAnn sym
                  , sym ~ WEB.ExprBuilder t st fs
                  , SC.Architecture arch' t
                  , SC.SymbolicArchitecture arch' t
                  , ext ~ CLI.LLVM arch
                  )
                => CCS.SolverOffline
                -> SC.OverrideConfig t (SC.LLVMPersonality sym) sym arch' ext
                -> LCS.GlobalVar CLM.Mem
                -> sym
                -> Ctx.Assignment (LCS.RegEntry sym) (Ctx.EmptyCtx Ctx.::> LCT.BVType 8 Ctx.::> CLT.LLVMPointerType (CLE.ArchWidth arch) Ctx.::> LCT.BVType 32)
                -> C.OverM SC.LLVMPersonality sym (CLI.LLVM arch) (LCS.RegValue sym LCT.UnitType)
do_debug_assert offSolver conf mvar sym (Ctx.Empty Ctx.:> p Ctx.:> pFile Ctx.:> line) = do
  cond <- liftIO $ WI.bvIsNonzero sym (regValue p)
  file <- lookupString mvar pFile
  l <- case WI.asBV (regValue line) of
         Just (BV.BV l)  -> return (fromInteger l)
         Nothing -> return 0
  let pos = WPL.SourcePos (T.pack file) l 0
  loc <- liftIO $ WI.getCurrentProgramLoc sym
  let loc' = loc{ WPL.plSourceLoc = pos }
  let msg = LCS.GenericSimError "crucible_debug_assert"
  liftIO $ LCB.addDurableAssertion sym (LCB.LabeledPred cond (LCS.SimError loc' msg))
  simState <- CMS.get
  let ng = WEB.exprCounter sym
  wantDebug <- CDS.withSolverAdapter offSolver $ \adapter -> do
    let logData = WS.defaultLogData
    negCond <- liftIO $ WI.notPred sym cond
    pathCond <- liftIO $ LCB.getPathCondition sym
    liftIO $ WS.solver_adapter_check_sat adapter sym logData [pathCond, negCond] $ \satRes ->
      case satRes of
        Sat (ev, _) -> do
          let simCtx = simState ^. LCSET.stateContext
          let model = simCtx ^. LCSET.cruciblePersonality . C.personalityModel
          modelVals <- CM.evalModel ev model
          let mv = C.ModelView modelVals
          return (WantDebug mv)
        _ -> return NoDebug
  case wantDebug of
    NoDebug -> return ()
    WantDebug simData -> do
      enterDebugger conf ng simState (SC.SuspendedAssertionFailure simData)

-- | This is the collection of debugger-specific overrides
debugOverrides :: ( LCB.IsSymInterface sym
                       , CLM.HasLLVMAnn sym
                       , CLM.HasPtrWidth wptr
                       , wptr ~ CLE.ArchWidth arch
                       , sym ~ WEB.ExprBuilder t st fs
                       , SC.Architecture arch' t
                       , SC.SymbolicArchitecture arch' t
                       , ext ~ CLI.LLVM arch
                       )
                    => C.CruxOptions
                    -> SC.OverrideConfig t (SC.LLVMPersonality sym) sym arch' ext
                    -> [CLI.OverrideTemplate (SC.LLVMPersonality sym) sym arch rtp l a]
debugOverrides cruxOpts sconf =
  [ CLI.basic_llvm_override $ [LCLQ.llvmOvr| void @crucible_breakpoint(i8*, ...) |]
      (do_breakpoint sconf)

  , CLI.basic_llvm_override $ [LCLQ.llvmOvr| void @crucible_debug_assert( i8, i8*, i32 ) |]
      (do_debug_assert (CDS.parseSolverOffline cruxOpts) sconf)
  ]
