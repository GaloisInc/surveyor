{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Surveyor.Core.Handlers.Context ( handleContextEvent ) where

import           Control.Lens ( (&), (^.), (.~), (%~), _Just )
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import qualified Data.Parameterized.Classes as PC
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.Nonce as PN
import           Fmt ( (+|), (||+) )
import qualified Fmt as Fmt

import qualified Surveyor.Core.Architecture as SCA
import qualified Surveyor.Core.Context as SCCx
import qualified Surveyor.Core.Events as SCE
import qualified Surveyor.Core.IRRepr as SCIR
import qualified Surveyor.Core.Logging as SCL
import qualified Surveyor.Core.Mode as SCM
import qualified Surveyor.Core.State as SCS
import qualified Surveyor.Core.SymbolicExecution as SymEx

handleContextEvent :: ( SCA.Architecture arch s
                      , SCA.CrucibleExtension arch
                      , MonadIO m
                      )
                  => SCS.S e u arch s
                  -> SCE.ContextEvent s (SCS.S e u)
                  -> m (SCS.State e u s)
handleContextEvent s0 evt =
  case evt of
    SCE.ViewBlock archNonce rep
      | Just PC.Refl <- PC.testEquality archNonce (s0 ^. SCS.lNonce) -> do
          -- Set the current view to the block viewer (of the appropriate IR)
          --
          -- Note that this doesn't manipulate the context at all
          liftIO $ SCS.logMessage s0 (SCL.msgWith { SCL.logLevel = SCL.Debug
                                                  , SCL.logSource = SCL.EventHandler "ViewBlock"
                                                  , SCL.logText = [Fmt.fmt ("Viewing a block for repr " +| rep ||+ "")]
                                                  })
          let s1 = s0 & SCS.lUIMode .~ SCM.SomeUIMode (SCM.BlockViewer archNonce rep)
          return $! SCS.State s1
      | otherwise -> return (SCS.State s0)
    SCE.ViewFunction archNonce rep -> do
      let s1 = s0 & SCS.lUIMode .~ SCM.SomeUIMode (SCM.FunctionViewer archNonce rep)
      return $! SCS.State s1
    SCE.ViewInstructionSemantics _archNonce -> do
      let s1 = s0 & SCS.lUIMode .~ SCM.SomeUIMode SCM.SemanticsViewer
      return $! SCS.State s1
    SCE.SelectNextInstruction archNonce ->
      withBlockViewer (return (SCS.State s0)) s0 $ \vnonce repr ->
        if | Just archState <- s0 ^. SCS.lArchState
           , cstk <- archState ^. SCS.contextG
           , Just PC.Refl <- PC.testEquality archNonce vnonce
           , Just PC.Refl <- PC.testEquality archNonce (s0 ^. SCS.lNonce)
           , Just SCS.ArchDict <- MapF.lookup repr (archState ^. SCS.archDictsG) -> do
               let s1 = s0 & SCS.lArchState ._Just . SCS.contextL .~ SCCx.selectNextInstruction repr cstk
               return $! SCS.State s1
           | otherwise -> return $! SCS.State s0
    SCE.SelectPreviousInstruction archNonce ->
      withBlockViewer (return (SCS.State s0)) s0 $ \vnonce repr ->
        if | Just archState <- s0 ^. SCS.lArchState
           , cstk <- archState ^. SCS.contextG
           , Just PC.Refl <- PC.testEquality archNonce vnonce
           , Just PC.Refl <- PC.testEquality archNonce (s0 ^. SCS.lNonce)
           , Just SCS.ArchDict <- MapF.lookup repr (archState ^. SCS.archDictsG) -> do
               let s1 = s0 & SCS.lArchState ._Just . SCS.contextL .~ SCCx.selectPreviousInstruction repr cstk
               return $! SCS.State s1
           | otherwise -> return $! SCS.State s0
    SCE.SelectNextOperand archNonce ->
      withBlockViewer (return (SCS.State s0)) s0 $ \vnonce repr ->
        if | Just archState <- s0 ^. SCS.lArchState
           , cstk <- archState ^. SCS.contextG
           , Just PC.Refl <- PC.testEquality archNonce vnonce
           , Just PC.Refl <- PC.testEquality archNonce (s0 ^. SCS.lNonce)
           , Just SCS.ArchDict <- MapF.lookup repr (archState ^. SCS.archDictsG) -> do
               let s1 = s0 & SCS.lArchState ._Just . SCS.contextL .~ SCCx.selectNextOperand repr cstk
               return $! SCS.State s1
           | otherwise -> return $! SCS.State s0
    SCE.SelectPreviousOperand archNonce ->
      withBlockViewer (return (SCS.State s0)) s0 $ \vnonce repr ->
        if | Just archState <- s0 ^. SCS.lArchState
           , cstk <- archState ^. SCS.contextG
           , Just PC.Refl <- PC.testEquality archNonce vnonce
           , Just PC.Refl <- PC.testEquality archNonce (s0 ^. SCS.lNonce)
           , Just SCS.ArchDict <- MapF.lookup repr (archState ^. SCS.archDictsG) -> do
               let s1 = s0 & SCS.lArchState ._Just . SCS.contextL .~ SCCx.selectPreviousOperand repr cstk
               return $! SCS.State s1
           | otherwise -> return $! SCS.State s0
    SCE.ResetInstructionSelection archNonce ->
      withBlockViewer (return (SCS.State s0)) s0 $ \vnonce repr ->
        if | Just archState <- s0 ^. SCS.lArchState
           , cstk <- archState ^. SCS.contextG
           , Just PC.Refl <- PC.testEquality archNonce vnonce
           , Just PC.Refl <- PC.testEquality archNonce (s0 ^. SCS.lNonce)
           , Just SCS.ArchDict <- MapF.lookup repr (archState ^. SCS.archDictsG) -> do
               let s1 = s0 & SCS.lArchState ._Just . SCS.contextL .~ SCCx.resetBlockSelection cstk
               return $! SCS.State s1
           | otherwise -> return $! SCS.State s0



    SCE.PushContext archNonce fh irrepr b
      | Just archState <- s0 ^. SCS.lArchState
      , Just PC.Refl <- PC.testEquality (s0 ^. SCS.lNonce) archNonce -> do
          let ng = SCS.sNonceGenerator s0
          (ctx, sessionState) <- liftIO $ SCCx.makeContext ng (archState ^. SCS.irCacheL) (archState ^. SCS.lAnalysisResult) fh irrepr b
          let s1 = s0 & SCS.lArchState . _Just . SCS.contextL %~ SCCx.pushContext ctx
                      & SCS.lArchState . _Just . SCS.symExStateL %~ SymEx.updateSessionState sessionState
          liftIO $ SCS.logMessage s0 (SCL.msgWith { SCL.logLevel = SCL.Debug
                                                  , SCL.logSource = SCL.EventHandler "PushContext"
                                                  , SCL.logText = [ Fmt.fmt ("Selecting block: " +| SCA.blockAddress b ||+ "")
                                                                  , Fmt.fmt ("from function " +| SCA.blockFunction b ||+ "")
                                                                  ]})
          return (SCS.State s1)
      | otherwise -> do
        case s0 ^. SCS.lArchState of
          Nothing -> liftIO $ SCS.logMessage s0 (SCL.msgWith { SCL.logText = ["No arch state"]
                                                             , SCL.logLevel = SCL.Warn
                                                             , SCL.logSource = SCL.EventHandler "PushContext"
                                                             })
          Just _archState
            | Just PC.Refl <- PC.testEquality (s0 ^. SCS.lNonce) archNonce ->
              return ()
            | otherwise ->
              liftIO $ SCS.logMessage s0 (SCL.msgWith { SCL.logText = ["Nonce mismatch"]
                                                      , SCL.logLevel = SCL.Warn
                                                      , SCL.logSource = SCL.EventHandler "PushContext"
                                                      })
        return (SCS.State s0)
    SCE.ContextBack -> do
      let s1 = s0 & SCS.lArchState . _Just . SCS.contextL %~ SCCx.contextBack
      return $! SCS.State s1
    SCE.ContextForward -> do
      let s1 = s0 & SCS.lArchState . _Just . SCS.contextL %~ SCCx.contextForward
      return $! SCS.State s1

withBlockViewer :: (SCA.Architecture arch s)
                => a
                -> SCS.S e u arch s
                -> (forall arch1 ir1 . PN.Nonce s arch1 -> SCIR.IRRepr arch1 ir1 -> a)
                -> a
withBlockViewer def s0 k =
  case s0 ^. SCS.lUIMode of
    SCM.SomeUIMode (SCM.BlockViewer vnonce repr) -> k vnonce repr
    SCM.SomeMiniBuffer (SCM.MiniBuffer m) ->
      case m of
        SCM.BlockViewer vnonce repr -> k vnonce repr
        _ -> def
    _ -> def
