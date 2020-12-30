{-# LANGUAGE GADTs #-}
module Surveyor.Core.Handlers.SymbolicExecution ( handleSymbolicExecutionEvent ) where

import qualified Control.Concurrent.Async as A
import           Control.Lens ( (^.), (^?), (.~), (&), (%~), _Just )
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import qualified Control.NF as NF
import qualified Data.Parameterized.Classes as PC
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Text as T
import qualified Lang.Crucible.Simulator.RegMap as CSR
import qualified Lang.Crucible.Types as LCT
import qualified What4.Expr as WEB

import qualified Surveyor.Core.Architecture as SCA
import qualified Surveyor.Core.Context as SCCx
import qualified Surveyor.Core.Events as SCE
import qualified Surveyor.Core.Logging as SCL
import qualified Surveyor.Core.Mode as SCM
import qualified Surveyor.Core.State as SCS
import qualified Surveyor.Core.SymbolicExecution as SymEx
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
