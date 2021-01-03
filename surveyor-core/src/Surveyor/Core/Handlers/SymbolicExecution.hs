{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module Surveyor.Core.Handlers.SymbolicExecution ( handleSymbolicExecutionEvent ) where

import qualified Control.Concurrent.Async as A
import qualified Control.Concurrent.Chan as CCC
import           Control.Lens ( (^.), (^?), (.~), (&), (%~), _Just )
import qualified Control.Lens as L
import           Control.Monad ( join )
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import qualified Control.NF as NF
import qualified Data.Foldable as F
import qualified Data.IORef as IOR
import           Data.Maybe ( isJust )
import qualified Data.Parameterized.Classes as PC
import qualified Data.Parameterized.Nonce as PN
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Sequence   as Seq
import qualified Data.Text as T
import qualified Lang.Crucible.Backend as CB
import qualified Lang.Crucible.CFG.Core as LCCC
import qualified Lang.Crucible.FunctionHandle as CFH
import qualified Lang.Crucible.Simulator.CallFrame as LCSC
import qualified Lang.Crucible.Simulator.ExecutionTree as LCSET
import qualified Lang.Crucible.Simulator.RegMap as CSR
import qualified Lang.Crucible.Types as LCT
import qualified What4.Expr.Builder as WEB

import qualified Surveyor.Core.Architecture as SCA
import qualified Surveyor.Core.Context as SCCx
import qualified Surveyor.Core.Events as SCE
import qualified Surveyor.Core.IRRepr as SCIR
import qualified Surveyor.Core.Logging as SCL
import qualified Surveyor.Core.Mode as SCM
import qualified Surveyor.Core.State as SCS
import qualified Surveyor.Core.SymbolicExecution as SymEx
import qualified Surveyor.Core.SymbolicExecution.ExecutionFeature as SCEF
import qualified Surveyor.Core.SymbolicExecution.Override as SEO
import qualified Surveyor.Core.SymbolicExecution.State as SES
import qualified Surveyor.Core.TranslationCache as SCTC
import qualified Surveyor.Core.ValueNames as SCV

handleSymbolicExecutionEvent :: (SCA.Architecture arch s, MonadIO m)
                             => SCS.S e u arch s
                             -> SCE.SymbolicExecutionEvent s (SCS.S e u)
                             -> m (SCS.State e u s)
handleSymbolicExecutionEvent s0 evt =
  case evt of
    SCE.InitializeSymbolicExecution archNonce mConfig mFuncHandle
      | Just PC.Refl <- PC.testEquality archNonce (s0 ^. SCS.lNonce)
      , Just sessionID <- s0 ^? SCS.lArchState . _Just . SCS.contextL . SCCx.currentContext . SCCx.symExecSessionIDL
      , Just symExSt <- s0 ^? SCS.lArchState . _Just . SCS.symExStateL -> do
          -- FIXME: Instead of the default, we could scan the context stack for
          -- the most recent configuration
          let ng = SCS.sNonceGenerator s0
          conf <- liftIO $ maybe (SymEx.defaultSymbolicExecutionConfig ng) return mConfig
          case SymEx.lookupSessionState symExSt sessionID of
            Just (Some oldState) -> liftIO $ SymEx.cleanupSymbolicExecutionState oldState
            Nothing -> return ()
          let newState = SymEx.configuringSymbolicExecution conf
          liftIO $ SCS.sEmitEvent s0 (SCE.UpdateSymbolicExecutionState archNonce newState)
          let s1 = s0 & SCS.lUIMode .~ SCM.SomeUIMode SCM.SymbolicExecutionManager
                      & SCS.lArchState . _Just . SCS.contextL . SCCx.currentContext . SCCx.symExecSessionIDL .~ SymEx.symbolicSessionID newState
          return (SCS.State s1)
      | otherwise -> return (SCS.State s0)

    SCE.BeginSymbolicExecutionSetup archNonce symExConfig cfg
      | Just PC.Refl <- PC.testEquality archNonce (s0 ^. SCS.lNonce) -> do
          let ng = SCS.sNonceGenerator s0
          symExSt <- liftIO $ SymEx.initializingSymbolicExecution ng symExConfig cfg
          liftIO $ SCS.sEmitEvent s0 (SCE.UpdateSymbolicExecutionState archNonce symExSt)
          let s1 = s0 & SCS.lUIMode .~ SCM.SomeUIMode SCM.SymbolicExecutionManager
          return (SCS.State s1)
      | otherwise -> return (SCS.State s0)

    SCE.StartSymbolicExecution archNonce ares symState
      | Just PC.Refl <- PC.testEquality archNonce (s0 ^. SCS.lNonce) -> do
        let eventChan = s0 ^. SCS.lEventChannel
        (newState, executionLoop) <- liftIO $ SymEx.startSymbolicExecution eventChan ares symState
        task <- liftIO $ A.async $ do
          inspectState <- executionLoop
          let updateUIMode () st =
                st & SCS.lUIMode .~ SCM.SomeUIMode SCM.SymbolicExecutionManager

          -- We emit the state update (so that it can be hooked), but also
          -- update the UI mode (via a separate state update)
          liftIO $ SCS.sEmitEvent s0 (SCE.UpdateSymbolicExecutionState archNonce inspectState)
          liftIO $ SCS.sEmitEvent s0 (SCE.AsyncStateUpdate archNonce (NF.nf ()) updateUIMode)

        -- Update the symbolic execution state (via an event), while updating
        -- the UI mode more directly.
        liftIO $ SCS.sEmitEvent s0 (SCE.UpdateSymbolicExecutionState archNonce newState)
        let s1 = s0 & SCS.lUIMode .~ SCM.SomeUIMode SCM.SymbolicExecutionManager
        return (SCS.State s1)
      | otherwise -> return (SCS.State s0)

    SCE.ReportSymbolicExecutionMetrics sid metrics -> do
      let s1 = s0 & SCS.lArchState . _Just . SCS.symExStateL %~ SymEx.updateSessionMetrics sid metrics
      return (SCS.State s1)

    SCE.NameValue valueNonce name -> do
      let s1 = s0 & SCS.lValueNames %~ SCV.addValueName valueNonce name
      return (SCS.State s1)

    SCE.InitializeValueNamePrompt archNonce name
      | Just PC.Refl <- PC.testEquality archNonce (s0 ^. SCS.lNonce)
      , Just sessionID <- s0 ^? SCS.lArchState . _Just . SCS.contextL . SCCx.currentContext . SCCx.symExecSessionIDL
      , Just symExSt <- s0 ^? SCS.lArchState . _Just . SCS.symExStateL
      , Just (Some (SymEx.Suspended _symNonce suspSt)) <- SymEx.lookupSessionState symExSt sessionID
      , Just (Some curVal) <- SymEx.suspendedCurrentValue suspSt -> do
          case LCT.asBaseType (CSR.regType curVal) of
            LCT.AsBaseType _btr ->
              case CSR.regValue curVal of
                WEB.AppExpr ae -> liftIO $ SCS.sEmitEvent s0 (SCE.NameValue (WEB.appExprId ae) name)
                WEB.NonceAppExpr nae -> liftIO $ SCS.sEmitEvent s0 (SCE.NameValue (WEB.nonceExprId nae) name)
                _ -> return ()
            LCT.NotBaseType -> return ()
          return (SCS.State s0)
      | otherwise -> do
          let msg = SCL.msgWith { SCL.logLevel = SCL.Debug
                                , SCL.logSource = SCL.EventHandler (T.pack "InitializeValueNamePrompt")
                                , SCL.logText = [T.pack "Pattern matches failed"]
                                }
          liftIO $ SCS.logMessage s0 msg
          return (SCS.State s0)

    SCE.UpdateSymbolicExecutionState archNonce newState
      | Just PC.Refl <- PC.testEquality archNonce (s0 ^. SCS.lNonce) -> do
          let msg = SCL.msgWith { SCL.logText = [T.pack "Updating symbolic execution state"
                                                , T.pack ("  Session ID is " ++ show (SymEx.symbolicSessionID newState))
                                                ]
                                }
          liftIO $ SCS.logMessage s0 msg
          let s1 = s0 & SCS.lArchState . _Just . SCS.symExStateL %~ SymEx.updateSessionState newState
          return (SCS.State s1)
      | otherwise -> return (SCS.State s0)

    SCE.SetCurrentSymbolicExecutionValue archNonce symNonce1 sessionID value
      | Just PC.Refl <- PC.testEquality archNonce (s0 ^. SCS.lNonce)
      , Just symExSt <- s0 ^? SCS.lArchState . _Just . SCS.symExStateL
      , Just (Some (SymEx.Suspended symNonce2 suspSt)) <- SymEx.lookupSessionState symExSt sessionID
      , Just PC.Refl <- PC.testEquality symNonce1 symNonce2 -> do
          -- NOTE: This does the update directly and does not go through the
          -- general symbolic state update to avoid triggering spurious UI updates
          let suspSt' = suspSt { SymEx.suspendedCurrentValue = value }
          let newState = SymEx.Suspended symNonce2 suspSt'
          let s1 = s0 & SCS.lArchState . _Just . SCS.symExStateL %~ SymEx.updateSessionState newState
          return (SCS.State s1)
      | otherwise -> return (SCS.State s0)

    SCE.DebugMonitorEvent archNonce sessionID (SCEF.CrucibleExecState _rtpNonce execState) returnChan debugConf
      | Just PC.Refl <- PC.testEquality archNonce (s0 ^. SCS.lNonce)
      , Just archState <- s0 ^. SCS.lArchState
      , Just (LCSET.SomeSimState simState) <- LCSET.execStateSimState execState -> do
          -- Note that the pattern guard above means that this is a no-op in the
          -- 'LCSET.ResultState' and 'LCSET.InitialState', as there is nothing
          -- that the debug UI can really do in those cases.
          --
          -- TODO In the 'LCSET.ResultState', we could instantiate the
          -- "Inspecting" state instead of the "Suspended" state
          let msg = SCL.msgWith { SCL.logText = [ T.pack "In DebugMonitorEvent"
                                                , T.pack ("  SessionID=" ++ show sessionID)
                                                ]
                                }
          liftIO $ SCS.logMessage s0 msg
          let resumeAction = CCC.writeChan returnChan SCEF.UnmodifiedExecState
          let reason = SES.SuspendedExecutionStep execState
          makeSuspendedState s0 archState archNonce sessionID simState reason resumeAction debugConf
      | otherwise -> return (SCS.State s0)
    SCE.OverrideMonitorEvent archNonce sessionID (SEO.CrucibleSimState _rtpNonce _argsNonce simState reason) returnChan debugConf
      | Just PC.Refl <- PC.testEquality archNonce (s0 ^. SCS.lNonce)
      , Just archState <- s0 ^. SCS.lArchState -> do
          let resumeAction = CCC.writeChan returnChan SEO.UnmodifiedSimState
          let msg = SCL.msgWith { SCL.logText = [ T.pack "In OverrideMonitorEvent"
                                                , T.pack ("  SessionID=" ++ show sessionID)
                                                ]
                                }
          liftIO $ SCS.logMessage s0 msg

          makeSuspendedState s0 archState archNonce sessionID simState reason resumeAction debugConf
      | otherwise -> return (SCS.State s0)

makeSuspendedState :: ( MonadIO m
                      , sym ~ WEB.ExprBuilder s st fs
                      , CB.IsSymInterface sym
                      , SCA.Architecture arch s
                      , ext ~ SCA.CrucibleExt arch
                      )
                   => SCS.S e u arch s
                   -> SCS.ArchState u arch s
                   -> PN.Nonce s arch
                   -> SymEx.SessionID s
                   -> LCSET.SimState p sym ext rtp f a
                   -> SES.SuspendedReason p sym ext rtp
                   -> IO ()
                   -> IOR.IORef SCEF.DebuggerFeatureState
                   -> m (SCS.State e u s)
makeSuspendedState s0 archState archNonce sessionID simState reason resumeAction debugConf = do
  -- Create a new suspended symbolic execution state based on what the
  -- override sent us.
  let topFrame = simState ^. LCSET.stateTree . LCSET.actFrame
  let sym = simState ^. LCSET.stateContext . LCSET.ctxSymInterface
  let symConf = SymEx.defaultSymbolicExecutionConfigWith sessionID
  let ng = s0 ^. SCS.lNonceGenerator
  let tc = archState ^. SCS.irCacheL
  let ares = archState ^. SCS.lAnalysisResult

  withParentFrameCFG (simState ^. LCSET.stateTree . L.to LCSET.activeFrames) $ \parentFrameCFG -> do
    contextStack <- liftIO $ contextStackFromState ng tc ares sessionID parentFrameCFG

    let symState = SES.SymbolicState { SES.symbolicConfig = symConf
                                     , SES.symbolicBackend = sym
                                     , SES.withSymConstraints = \x -> x
                                     , SES.someCFG = LCCC.SomeCFG parentFrameCFG
                                     , SES.symbolicGlobals = topFrame ^. LCSET.gpGlobals
                                     -- FIXME: Factor this out from here
                                     -- - we don't need it in this case
                                     , SES.symbolicRegs = error "Initial symbolic regs"
                                     }
    let msg = SCL.msgWith { SCL.logText = [T.pack "Making a suspended state"]
                          }
    liftIO $ SCS.logMessage s0 msg
    symExState <- liftIO $ SymEx.suspendedState (WEB.exprCounter sym) symState simState reason resumeAction debugConf

    liftIO $ SCS.sEmitEvent s0 (SCE.UpdateSymbolicExecutionState archNonce symExState)
    -- We are updating the symbolic execution state via a dedicated
    -- message, but need to update the active session ID in the context
    -- separately
    let s1 = s0 & SCS.lArchState . _Just . SCS.contextL .~ contextStack
                & SCS.lUIMode .~ SCM.SomeUIMode SCM.SymbolicExecutionManager
    let curSesId = s1 ^? SCS.lArchState . _Just . SCS.contextL . SCCx.currentContext . SCCx.symExecSessionIDL
    let msg2 = SCL.msgWith { SCL.logText = [ T.pack ("  Current session ID is " ++ show curSesId)
                                           ]
                           }
    liftIO $ SCS.logMessage s0 msg2
    return (SCS.State s1)

contextStackFromState :: forall arch s ext blocks initialArgs ret
                       . ( SCA.Architecture arch s
                         , ext ~ SCA.CrucibleExt arch
                         )
                      => PN.NonceGenerator IO s
                      -> SCTC.TranslationCache arch s
                      -> SCA.AnalysisResult arch s
                      -> SymEx.SessionID s
                      -> LCCC.CFG ext blocks initialArgs ret
                      -> IO (SCCx.ContextStack arch s)
contextStackFromState ng tc ares sesID cfg = do
  allCfgs <- mapM toCFG (SCA.functions ares)
  case join (F.find (matchesCFG cfg) allCfgs) of
    Nothing -> return SCCx.emptyContextStack
    Just (fh, LCCC.AnyCFG _) -> do
      mAltIR <- SCA.asAlternativeIR SCIR.CrucibleRepr ares fh
      case mAltIR of
        Nothing -> return SCCx.emptyContextStack
        Just ([], _) -> return SCCx.emptyContextStack
        Just (b0:_, _blockMap) -> do
          -- 'makeContext' returns a symbolic execution context; we actually
          -- have our own we want to use instead
          (curCtx, _sid) <- SCCx.makeContext ng tc ares fh SCIR.CrucibleRepr b0
          let curCtx' = curCtx { SCCx.cSymExecSessionID = sesID }
          return SCCx.ContextStack { SCCx.cStack = Seq.singleton curCtx'
                                   , SCCx.cStackIndex = Nothing
                                   }
  where
    withCfgNonce c k = k (CFH.handleID (LCCC.cfgHandle c))

    matchesCFG _ Nothing = False
    matchesCFG targetCfg (Just (_fh, LCCC.AnyCFG thisCfg)) =
      withCfgNonce targetCfg $ \targetNonce ->
        withCfgNonce thisCfg $ \thisNonce -> isJust (PC.testEquality thisNonce targetNonce)

    toCFG fh = do
      mcfg <- SCA.crucibleCFG ares fh
      return ((fh,) <$> mcfg)


withParentFrameCFG :: [LCSET.SomeFrame (LCSC.SimFrame sym ext)]
                   -> (forall blocks init ret . LCCC.CFG ext blocks init ret -> t)
                   -> t
withParentFrameCFG fs k =
  case fs of
    [] -> error "No parent frame"
    LCSET.SomeFrame (LCSC.MF LCSC.CallFrame { LCSC._frameCFG = pfcfg }) : _ -> k pfcfg
    _ : _fs -> withParentFrameCFG _fs k
