{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Surveyor.Core.Handlers.Debugging ( handleDebuggingEvent ) where

import           Control.Lens ( (^.), (^?), _Just, (.~), (&), (%~) )
import qualified Control.Lens as L
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Parameterized.Context as Ctx
import qualified Data.IORef as IOR
import qualified System.Process as SP

import qualified Lang.Crucible.Backend as CB
import qualified Lang.Crucible.CFG.Core as CCC
import qualified Lang.Crucible.CFG.Extension as CCE
import qualified Lang.Crucible.Simulator as CS
import qualified Lang.Crucible.Simulator.ExecutionTree as CSET
import qualified Lang.Crucible.Simulator.EvalStmt as CSE
import qualified Lang.Crucible.Simulator.RegMap as CSRM
import qualified Lang.Crucible.Simulator.Operations as CSO
import qualified Lang.Crucible.Simulator.CallFrame as CSCF
import qualified Lang.Crucible.Types as CT

import qualified What4.Config as WC
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
      , Just ares <- s0 ^? SCS.lArchState . _Just . SCS.lAnalysisResult
      , Just symExSt <- s0 ^? SCS.lArchState . _Just . SCS.symExStateL
      , ng <- s0 ^. SCS.lNonceGenerator
      , Just (Some exst@(SymEx.Suspended symNonce suspSt)) <- SymEx.lookupSessionState symExSt sessionID -> do
          let st = SymEx.suspendedSymState suspSt
          let sym = SymEx.symbolicBackend st
          case SymEx.someCFG st of
            CCC.SomeCFG cfg -> SymEx.withSymConstraints st . liftIO $ do

              let simState0 = SymEx.suspendedSimState suspSt

              let vff = simState0 ^. CSET.stateTree . CSET.actContext
              let topFrame = simState0 ^. CSET.stateTree . CSET.actFrame
              let simFrame = topFrame ^. CSET.gpValue

              case vff of
                CSET.VFFEnd vfv@(CSET.VFVCall ctx (CSCF.MF frm) (CSET.ReturnToCrucible rtp stmts)) ->
                  case rtp of
                    CT.UnitRepr -> do
                      let unit = CSRM.RegEntry CT.UnitRepr ()
                      let cont = CSO.performReturn "debug_assert" vfv unit
                      res <- CSE.advanceCrucibleState cont simState0
                      case CSET.execStateSimState res of
                        Nothing -> error "Failed to execute crucible step"
                        Just (CSET.SomeSimState simState1) -> do
                          let st' = SymEx.Suspended symNonce (suspSt { SymEx.suspendedSimState = simState1 })
                          return $! SCS.State (s0 & SCS.lArchState . _Just . SCS.symExStateL
                                                  %~ SymEx.mergeSessionState (SymEx.singleSessionState st'))
{-
              simulatorState1 <- case vff of
                CSET.VFFEnd vfv@(CSET.VFVCall _ cf _) ->
                  case cf of
                    CSCF.MF crucCallFrame ->
                      case CSCF.frameReturnType crucCallFrame of
                        CCC.UnitRepr -> do
                          let cont = CSO.performReturn "debug_assert" vfv (CSRM.RegEntry CCC.UnitRepr ())
                          res <- CSE.advanceCrucibleState cont simState0
                          case CSET.execStateSimState res of
                            Just (CSET.SomeSimState simState1) ->
                              return $! SymEx.Suspended symNonce (suspSt { SymEx.suspendedSimState = simState1 })
                            Nothing -> return exst


              return $! SCS.State (s0 & SCS.lArchState . _Just . SCS.symExStateL
                                   %~ SymEx.mergeSessionState (SymEx.singleSessionState simulatorState1))
-}
      | otherwise -> return $! SCS.State s0
