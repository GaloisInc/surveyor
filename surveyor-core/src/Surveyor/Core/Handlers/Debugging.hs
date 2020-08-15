{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Surveyor.Core.Handlers.Debugging ( handleDebuggingEvent ) where

import           Control.Lens ( (^.), (^?), _Just, (&), (%~) )
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import           Control.Monad.Reader ( runReaderT )
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.BitVector.Sized as BV

import qualified Lang.Crucible.Simulator.ExecutionTree as CSET
import qualified Lang.Crucible.Simulator.EvalStmt as CSE
import qualified Lang.Crucible.Simulator.RegMap as CSRM
import qualified Lang.Crucible.Simulator.Operations as CSO
import qualified Lang.Crucible.Simulator.CallFrame as CSCF
import qualified Lang.Crucible.Types as CT
import qualified Lang.Crucible.LLVM.MemModel as CLT
import qualified What4.Interface as WI

import qualified Surveyor.Core.Architecture as SCA
import qualified Surveyor.Core.Events as SCE
import qualified Surveyor.Core.State as SCS
import qualified Surveyor.Core.Context as CCX
import qualified Surveyor.Core.SymbolicExecution as SymEx

handleDebuggingEvent :: (SCA.Architecture arch s, MonadIO m)
                     => SCS.S e u arch s
                     -> SCE.DebuggingEvent s (SCS.S e u)
                     -> m (SCS.State e u s)
handleDebuggingEvent s0 evt =
  case evt of
    SCE.StepExecution
      | Just sessionID <- s0 ^? SCS.lArchState . _Just . SCS.contextL . CCX.currentContext . CCX.symExecSessionIDL
      , Just symExSt <- s0 ^? SCS.lArchState . _Just . SCS.symExStateL
      , Just (Some (SymEx.Suspended symNonce suspSt)) <- SymEx.lookupSessionState symExSt sessionID -> do
          let st = SymEx.suspendedSymState suspSt
          let sym = SymEx.symbolicBackend st
          liftIO $! do
              let simState0 = SymEx.suspendedSimState suspSt
              let vff = simState0 ^. CSET.stateTree . CSET.actContext

              case vff of
                CSET.VFFEnd vfv@(CSET.VFVCall _ (CSCF.MF frm) (CSET.ReturnToCrucible rtp _)) -> do
                  case rtp of
                    CT.UnitRepr -> do
                      let ret = CSRM.RegEntry CT.UnitRepr ()
                      let cont = CSO.performReturn "debug_assert" vfv ret
                      exst <- runReaderT cont simState0
                      res <- CSE.singleStepCrucible 0 exst

                      case CSET.execStateSimState res of
                        Nothing -> error "Failed to execute crucible step"
                        Just (CSET.SomeSimState simState1) -> do
                          let topFrame = simState1 ^. CSET.stateTree . CSET.actFrame
                          case topFrame ^. CSET.gpValue of
                            CSCF.MF cf -> do
                              let st' = SymEx.Suspended symNonce (suspSt { SymEx.suspendedSimState = simState1
                                                                         , SymEx.suspendedCallFrame = cf })
                              return $! SCS.State (s0 & SCS.lArchState . _Just . SCS.symExStateL
                                                      %~ SymEx.mergeSessionState (SymEx.singleSessionState st'))

                            _ -> error "Unexpected frame after stepping execution"

                    CLT.LLVMPointerRepr w -> do
                      blk0 <- WI.natLit sym 0
                      bv0 <- WI.bvLit sym w (BV.mkBV w 32)
                      let ptr = CLT.LLVMPointer blk0 bv0
                      let ret = CSRM.RegEntry rtp ptr
                      let cont = CSO.performReturn "debug_assert" vfv ret
                      exst <- runReaderT cont simState0
                      res <- CSE.singleStepCrucible 0 exst

                      case CSET.execStateSimState res of
                        Nothing -> error "Failed to execute crucible step"
                        Just (CSET.SomeSimState simState1) -> do
                          let topFrame = simState1 ^. CSET.stateTree . CSET.actFrame
                          case topFrame ^. CSET.gpValue of
                            CSCF.MF cf -> do
                              let st' = SymEx.Suspended symNonce (suspSt { SymEx.suspendedSimState = simState1
                                                                         , SymEx.suspendedCallFrame = cf })
                              return $! SCS.State (s0 & SCS.lArchState . _Just . SCS.symExStateL
                                                      %~ SymEx.mergeSessionState (SymEx.singleSessionState st'))

                            _ -> error "Unexpected frame after stepping execution"

                    _ ->
                      case CSCF.frameHandle frm of
                        CSCF.SomeHandle fh ->
                          error $! "Unexpected frame: " ++ show fh ++ " return type: " ++ show rtp

                CSET.VFFBranch {} ->
                  error "Unexpected active frame: VFFBranch"
                CSET.VFFPartial {} ->
                  error "Unexpected active frame: VFFPartial"
                CSET.VFFEnd {} ->
                  error "Unexpected active frame: VFFEnd"

      | otherwise -> return $! SCS.State s0
