{-# LANGUAGE GADTs #-}
module Surveyor.Brick.Handlers.SymbolicExecution ( handleSymbolicExecutionEvent ) where

import qualified Brick as B
import qualified Control.Concurrent.Async as A
import           Control.Lens ( (&), (^.), (.~), (%~), _Just, (^?) )
import           Control.Monad.IO.Class ( liftIO )
import qualified Control.NF as NF
import qualified Data.Parameterized.Classes as PC
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Text as T
import qualified Lang.Crucible.Simulator.RegMap as CSR
import qualified Lang.Crucible.Types as LCT
import           Surveyor.Brick.Names ( Names(..) )
import qualified Surveyor.Core as C
import qualified What4.Expr.Builder as WEB

import qualified Surveyor.Brick.Extension as SBE
import qualified Surveyor.Brick.Widget.SymbolicExecution as SEM


handleSymbolicExecutionEvent :: (C.Architecture arch s)
                             => C.S SBE.BrickUIExtension SBE.BrickUIState arch s
                             -> C.SymbolicExecutionEvent s (C.S SBE.BrickUIExtension SBE.BrickUIState)
                             -> B.EventM Names (B.Next (C.State SBE.BrickUIExtension SBE.BrickUIState s))
handleSymbolicExecutionEvent s0 evt =
  case evt of
    C.InitializeSymbolicExecution archNonce mConfig mFuncHandle
      | Just PC.Refl <- PC.testEquality archNonce (s0 ^. C.lNonce)
      , Just sessionID <- s0 ^? C.lArchState . _Just . C.contextL . C.currentContext . C.symExecSessionIDL
      , Just symExSt <- s0 ^? C.lArchState . _Just . C.symExStateL -> do
          -- FIXME: Instead of the default, we could scan the context stack for
          -- the most recent configuration
          let ng = C.sNonceGenerator s0
          conf <- liftIO $ maybe (C.defaultSymbolicExecutionConfig ng) return mConfig
          case C.lookupSessionState symExSt sessionID of
            Just (Some oldState) -> liftIO $ C.cleanupSymbolicExecutionState oldState
            Nothing -> return ()
          let newState = C.configuringSymbolicExecution conf
          let manager = SEM.symbolicExecutionManager (Some newState)
          let s1 = s0 & C.lUIMode .~ C.SomeUIMode C.SymbolicExecutionManager
                      & C.lArchState . _Just . C.symExStateL %~ (<> C.singleSessionState newState)
                      & C.lArchState . _Just . C.lUIState . SBE.symbolicExecutionManagerL .~ manager
          B.continue (C.State s1)
      | otherwise -> B.continue (C.State s0)

    C.BeginSymbolicExecutionSetup archNonce symExConfig cfg
      | Just PC.Refl <- PC.testEquality archNonce (s0 ^. C.lNonce) -> do
          let ng = C.sNonceGenerator s0
          symExSt <- liftIO $ C.initializingSymbolicExecution ng symExConfig cfg
          let manager = SEM.symbolicExecutionManager (Some symExSt)
          let s1 = s0 & C.lUIMode .~ C.SomeUIMode C.SymbolicExecutionManager
                      & C.lArchState . _Just . C.lUIState . SBE.symbolicExecutionManagerL .~ manager
                      & C.lArchState . _Just . C.symExStateL %~ (<> C.singleSessionState symExSt)
          B.continue (C.State s1)
      | otherwise -> B.continue (C.State s0)
    C.StartSymbolicExecution archNonce ares symState
      | Just PC.Refl <- PC.testEquality archNonce (s0 ^. C.lNonce) -> do
        let eventChan = s0 ^. C.lEventChannel
        (newState, executionLoop) <- liftIO $ C.startSymbolicExecution eventChan ares symState
        task <- liftIO $ A.async $ do
          inspectState <- executionLoop
          let updateSymExecState _ st =
                let manager = SEM.symbolicExecutionManager (Some inspectState)
                in st & C.lArchState . _Just . C.symExStateL %~ (<> C.singleSessionState newState)
                      & C.lArchState . _Just . C.lUIState . SBE.symbolicExecutionManagerL .~ manager
                      & C.lUIMode .~ C.SomeUIMode C.SymbolicExecutionManager
          -- We pass () as the value of the update state and capture the real
          -- value (the new state) because there isn't an easy way to get an
          -- NFData instance for states.  That is okay, though, because they are
          -- evaluated enough.
          C.writeChan eventChan (C.AsyncStateUpdate archNonce (NF.nf ()) updateSymExecState)
        let manager = SEM.symbolicExecutionManager (Some newState)
        let s1 = s0 & C.lUIMode .~ C.SomeUIMode C.SymbolicExecutionManager
                    & C.lArchState . _Just . C.lUIState . SBE.symbolicExecutionManagerL .~ manager
                    & C.lArchState . _Just . C.symExStateL %~ (<> C.singleSessionState newState)
        B.continue (C.State s1)
      | otherwise -> B.continue (C.State s0)
    C.ReportSymbolicExecutionMetrics sid metrics -> do
      let s1 = s0 & C.lArchState . _Just . C.symExStateL %~ C.updateSessionMetrics sid metrics
      B.continue (C.State s1)

    C.NameValue valueNonce name -> do
      let s1 = s0 & C.lValueNames %~ C.addValueName valueNonce name
      B.continue (C.State s1)

    C.InitializeValueNamePrompt archNonce name
      | Just PC.Refl <- PC.testEquality archNonce (s0 ^. C.lNonce)
      , Just sessionID <- s0 ^? C.lArchState . _Just . C.contextL . C.currentContext . C.symExecSessionIDL
      , Just symExSt <- s0 ^? C.lArchState . _Just . C.symExStateL
      , Just (Some (C.Suspended _symNonce suspSt)) <- C.lookupSessionState symExSt sessionID
      , Just (Some curVal) <- C.suspendedCurrentValue suspSt -> do
          case LCT.asBaseType (CSR.regType curVal) of
            LCT.AsBaseType _btr ->
              case CSR.regValue curVal of
                WEB.AppExpr ae -> do
                  liftIO $ C.logMessage s0 (C.msgWith { C.logLevel = C.Debug
                                                      , C.logSource = C.EventHandler (T.pack "InitializeValueNamePrompt")
                                                      , C.logText = [T.pack "Name: " <> name]
                                                      })
                  liftIO $ C.sEmitEvent s0 (C.NameValue (WEB.appExprId ae) name)
                WEB.NonceAppExpr nae -> liftIO $ C.sEmitEvent s0 (C.NameValue (WEB.nonceExprId nae) name)
                _ -> return ()
            LCT.NotBaseType -> return ()
          B.continue (C.State s0)
      | Just PC.Refl <- PC.testEquality archNonce (s0 ^. C.lNonce)
      , Just sessionID <- s0 ^? C.lArchState . _Just . C.contextL . C.currentContext . C.symExecSessionIDL
      , Just symExSt <- s0 ^? C.lArchState . _Just . C.symExStateL
      , Just (Some (C.Suspended _symNonce suspSt)) <- C.lookupSessionState symExSt sessionID ->
        case C.suspendedCurrentValue suspSt of
          Nothing -> do
            liftIO $ C.logMessage s0 (C.msgWith { C.logLevel = C.Debug
                                              , C.logSource = C.EventHandler (T.pack "InitializeValueNamePrompt")
                                              , C.logText = [T.pack "Empty suspended state"]
                                              })
            B.continue (C.State s0)
          Just _ -> error ""
      | otherwise -> do
          liftIO $ C.logMessage s0 (C.msgWith { C.logLevel = C.Debug
                                              , C.logSource = C.EventHandler (T.pack "InitializeValueNamePrompt")
                                              , C.logText = [T.pack "Pattern matches failed"]
                                              })
          B.continue (C.State s0)
