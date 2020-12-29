{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Surveyor.Core.Handlers.Debugging ( handleDebuggingEvent ) where

import           Control.Lens ( (^.), (^?), _Just, (&), (%~) )
import qualified Control.Lens as L
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import           Control.Monad.Reader ( runReaderT )
import qualified Data.BitVector.Sized as BV
import qualified Data.Parameterized.Nonce as PN
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Text as T
import           GHC.Stack ( HasCallStack )
import qualified Prettyprinter as PP

import qualified Lang.Crucible.Backend as CB
import qualified Lang.Crucible.Simulator.ExecutionTree as CSET
import qualified Lang.Crucible.Simulator.EvalStmt as CSE
import qualified Lang.Crucible.Simulator.RegMap as CSRM
import qualified Lang.Crucible.Simulator.Operations as CSO
import qualified Lang.Crucible.Simulator.CallFrame as CSCF
import qualified Lang.Crucible.Types as CT
import qualified Lang.Crucible.LLVM.MemModel as CLT
import qualified What4.Expr as WE
import qualified What4.FunctionName as WFN
import qualified What4.Interface as WI
import qualified What4.ProgramLoc as WPL

import qualified Surveyor.Core.Architecture as SCA
import qualified Surveyor.Core.Context as CCX
import qualified Surveyor.Core.Events as SCE
import qualified Surveyor.Core.Logging as SCL
import qualified Surveyor.Core.State as SCS
import qualified Surveyor.Core.SymbolicExecution as SymEx

stepExecution :: ( ext ~ SCA.CrucibleExt arch
                 , SCA.Architecture arch s
                 , CB.IsSymInterface sym
                 , sym ~ WE.ExprBuilder s st fs
                 , MonadIO m
                 , HasCallStack
                 )
              => SCS.S e u arch s
              -> PN.Nonce s sym
              -> SymEx.SuspendedState sym init reg p ext args blocks ret1 rtp f a ctx arch s
              -> CSET.SimState p sym ext ret f a
              -> CSET.ValueFromFrame p sym ext ret caller
              -> m (SCS.State e u s)
stepExecution s0 symNonce suspSt simState0 vff =
  case vff of
    CSET.VFFEnd vfv -> do
      logExecutionState s0 (PP.pretty "VFFEnd")
      case vfv of
        CSET.VFVEnd -> do
          logExecutionState s0 (PP.pretty "VFVEnd")
          return $! SCS.State s0
        CSET.VFVPartial vfv' loc p abortedRes -> do
          logExecutionState s0 (PP.pretty "VFVPartial")
          return $! SCS.State s0
        CSET.VFVCall vff' simFrame retHdlr -> do
          logExecutionState s0 (PP.pretty "VFVCall")
          case retHdlr of
            CSET.ReturnToOverride {} -> do
              logExecutionState s0 (PP.pretty "ReturnToOverride")
              return $! SCS.State s0
            CSET.ReturnToCrucible retRepr stmts -> do
              logExecutionState s0 (PP.pretty "ReturnToCrucible" <> PP.parens (PP.pretty (show retRepr)))
              logExecutionState s0 (PP.pretty "  Returning to " <> PP.pretty vfv)
              case retRepr of
                CT.UnitRepr -> do
                  -- In this case, the frame we are paused in has a unit return
                  let ret = CSRM.RegEntry CT.UnitRepr ()
                  let fnName = WFN.functionNameFromText (T.pack "debug_assert")
                  let cont = CSET.ReturnState fnName vfv ret simState0
                        -- CSO.performReturn fnName vfv ret
                  -- exst <- liftIO $ runReaderT cont simState0
                  res <- liftIO $ CSE.singleStepCrucible 0 cont

                  case CSET.execStateSimState res of
                    Nothing -> do
                      -- NOTE: This could happen if the user has stepped to
                      -- a result state (i.e., symbolic execution has
                      -- finished).  We might want to display something else
                      -- at that point.
                      logExecutionState s0 (PP.pretty "Failed to step the crucible state; execution has stopped")
                      return $! SCS.State s0
                    Just (CSET.SomeSimState simState1) -> do
                      let topFrame = simState1 ^. CSET.stateTree . CSET.actFrame
                      case topFrame ^. CSET.gpValue of
                        CSCF.MF cf -> do
                          let fname = (CSCF.MF cf) ^. CSCF.frameFunctionName
                          logExecutionState s0 (PP.pretty "Stepped to a new state in " <> PP.pretty (show fname))
                          let newState = suspSt { SymEx.suspendedSimState = simState1
                                                , SymEx.suspendedCallFrame = cf
                                                }
                          let archNonce = s0 ^. SCS.lNonce
                          let st' = SymEx.Suspended symNonce newState
                          liftIO $ SCS.sEmitEvent s0 (SCE.UpdateSymbolicExecutionState archNonce st')
                          return $! SCS.State s0

                        _ -> error "Unexpected frame after stepping execution"
                _ -> do
                  logExecutionState s0 (PP.pretty "Unsupported return type in frame: " <> PP.viaShow retRepr)
                  return $! SCS.State s0
            CSET.TailReturnToCrucible {} -> do
              logExecutionState s0 (PP.pretty "TailReturnToCrucible")
              return $! SCS.State s0
    CSET.VFFPartial {} -> do
      logExecutionState s0 (PP.pretty "VFFPartial")
      return $! SCS.State s0
    CSET.VFFBranch {} -> do
      logExecutionState s0 (PP.pretty "VFFBranch")
      return $! SCS.State s0

logExecutionState :: (MonadIO m, HasCallStack)
                  => SCS.S e u arch s
                  -> PP.Doc ann
                  -> m ()
logExecutionState s0 msgText = do
  let msg = SCL.msgWithContext { SCL.logLevel = SCL.Info
                               , SCL.logSource = SCL.EventHandler (T.pack "StepExecution")
                               , SCL.logText = [ T.pack (show msgText) ]
                               }
  liftIO $ SCS.logMessage s0 msg

handleDebuggingEvent :: (SCA.Architecture arch s, MonadIO m, HasCallStack)
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
          let simState0 = SymEx.suspendedSimState suspSt
          let vff = simState0 ^. CSET.stateTree . CSET.actContext
          stepExecution s0 symNonce suspSt simState0 vff
      | otherwise -> return $! SCS.State s0
