{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Surveyor.Brick.Handlers.Context ( handleContextEvent ) where

import qualified Brick as B
import           Control.Lens ( (&), (^.), (.~), (%~), _Just )
import           Control.Monad.IO.Class ( liftIO )
import qualified Data.Parameterized.Classes as PC
import qualified Data.Parameterized.Nonce as PN
import           Fmt ( (+|), (|+), (||+) )
import qualified Fmt as Fmt
import           Surveyor.Brick.Names ( Names(..) )
import qualified Surveyor.Core as C

import           Surveyor.Brick.Attributes ( focusedListAttr )
import qualified Surveyor.Brick.Extension as SBE
import qualified Surveyor.Brick.Widget.BlockSelector as BS
import qualified Surveyor.Brick.Widget.BlockViewer as BV
import qualified Surveyor.Brick.Widget.FunctionSelector as FS

handleContextEvent :: (C.Architecture arch s)
                  => C.S SBE.BrickUIExtension SBE.BrickUIState arch s
                  -> C.ContextEvent s (C.S SBE.BrickUIExtension SBE.BrickUIState)
                  -> B.EventM Names (B.Next (C.State SBE.BrickUIExtension SBE.BrickUIState s))
handleContextEvent s0 evt =
  case evt of
    C.ViewBlock archNonce rep
      | Just PC.Refl <- PC.testEquality archNonce (s0 ^. C.lNonce) -> do
          -- Set the current view to the block viewer (of the appropriate IR)
          --
          -- Note that this doesn't manipulate the context at all
          liftIO $ C.logMessage s0 (C.msgWith { C.logLevel = C.Debug
                                              , C.logSource = C.EventHandler "ViewBlock"
                                              , C.logText = [Fmt.fmt ("Viewing a block for repr " +| rep ||+ "")]
                                              })
          let s1 = s0 & C.lUIMode .~ C.SomeUIMode (C.BlockViewer archNonce rep)
          B.continue $! C.State s1
      | otherwise -> B.continue (C.State s0)
    C.ViewFunction archNonce rep -> do
      let s1 = s0 & C.lUIMode .~ C.SomeUIMode (C.FunctionViewer archNonce rep)
      B.continue $! C.State s1
    C.ViewInstructionSemantics _archNonce -> do
      let s1 = s0 & C.lUIMode .~ C.SomeUIMode C.SemanticsViewer
      B.continue $! C.State s1
    C.SelectNextInstruction archNonce ->
      withBlockViewer s0 $ \vnonce repr ->
        if | Just archState <- s0 ^. C.lArchState
           , cstk <- archState ^. C.contextG
           , Just PC.Refl <- PC.testEquality archNonce vnonce
           , Just PC.Refl <- PC.testEquality archNonce (s0 ^. C.lNonce) ->
             case archState ^. SBE.blockViewerG repr of
               Nothing -> error "Inconsistent block viewers"
               Just bv -> BV.withBlockViewerConstraints bv $ do
                 let s1 = s0 & C.lArchState ._Just . C.contextL .~ C.selectNextInstruction repr cstk
                 B.continue $! C.State s1
           | otherwise -> B.continue $! C.State s0
    C.SelectPreviousInstruction archNonce ->
      withBlockViewer s0 $ \vnonce repr ->
        if | Just archState <- s0 ^. C.lArchState
           , cstk <- archState ^. C.contextG
           , Just PC.Refl <- PC.testEquality archNonce vnonce
           , Just PC.Refl <- PC.testEquality archNonce (s0 ^. C.lNonce) ->
             case archState ^. SBE.blockViewerG repr of
               Nothing -> error "Inconsistent block viewers"
               Just bv -> BV.withBlockViewerConstraints bv $ do
                 let s1 = s0 & C.lArchState ._Just . C.contextL .~ C.selectPreviousInstruction repr cstk
                 B.continue $! C.State s1
           | otherwise -> B.continue $! C.State s0
    C.SelectNextOperand archNonce ->
      withBlockViewer s0 $ \vnonce repr ->
        if | Just archState <- s0 ^. C.lArchState
           , cstk <- archState ^. C.contextG
           , Just PC.Refl <- PC.testEquality archNonce vnonce
           , Just PC.Refl <- PC.testEquality archNonce (s0 ^. C.lNonce) ->
             case archState ^. SBE.blockViewerG repr of
               Nothing -> error "Inconsistent block viewers"
               Just bv -> BV.withBlockViewerConstraints bv $ do
                 let s1 = s0 & C.lArchState ._Just . C.contextL .~ C.selectNextOperand repr cstk
                 B.continue $! C.State s1
           | otherwise -> B.continue $! C.State s0
    C.SelectPreviousOperand archNonce ->
      withBlockViewer s0 $ \vnonce repr ->
        if | Just archState <- s0 ^. C.lArchState
           , cstk <- archState ^. C.contextG
           , Just PC.Refl <- PC.testEquality archNonce vnonce
           , Just PC.Refl <- PC.testEquality archNonce (s0 ^. C.lNonce) ->
             case archState ^. SBE.blockViewerG repr of
               Nothing -> error "Inconsistent block viewers"
               Just bv -> BV.withBlockViewerConstraints bv $ do
                 let s1 = s0 & C.lArchState ._Just . C.contextL .~ C.selectPreviousOperand repr cstk
                 B.continue $! C.State s1
           | otherwise -> B.continue $! C.State s0
    C.ResetInstructionSelection archNonce ->
      withBlockViewer s0 $ \vnonce repr ->
        if | Just archState <- s0 ^. C.lArchState
           , cstk <- archState ^. C.contextG
           , Just PC.Refl <- PC.testEquality archNonce vnonce
           , Just PC.Refl <- PC.testEquality archNonce (s0 ^. C.lNonce) ->
             case archState ^. SBE.blockViewerG repr of
               Nothing -> error "Inconsistent block viewers"
               Just bv -> BV.withBlockViewerConstraints bv $ do
                 let s1 = s0 & C.lArchState ._Just . C.contextL .~ C.resetBlockSelection cstk
                 B.continue $! C.State s1
           | otherwise -> B.continue $! C.State s0

    C.FindBlockContaining archNonce addr
      | Just archState <- s0 ^. C.lArchState
      , ares <- archState ^. C.lAnalysisResult
      , Just PC.Refl <- PC.testEquality (s0 ^. C.lNonce) archNonce -> do
          liftIO $ C.logMessage s0 (C.msgWith { C.logLevel = C.Debug
                                              , C.logSource = C.EventHandler "FindBlockContaining"
                                              , C.logText = [Fmt.fmt ("Finding block at address " +| C.prettyAddress addr |+ "")]
                                              })
          case C.containingBlocks ares addr of
            [b] -> do
              let fh = C.blockFunction b
              liftIO (C.sEmitEvent s0 (C.PushContext archNonce fh C.BaseRepr b))
              liftIO (C.sEmitEvent s0 (C.ViewBlock archNonce C.BaseRepr))
              B.continue (C.State s0)
            blocks -> do
              liftIO (C.sEmitEvent s0 (C.ListBlocks archNonce blocks))
              B.continue (C.State s0)
      | otherwise -> B.continue (C.State s0)
    C.ListBlocks archNonce blocks
      | Just PC.Refl <- PC.testEquality (s0 ^. C.lNonce) archNonce -> do
          let callback b = do
                let fh = C.blockFunction b
                C.logMessage s0 (C.msgWith { C.logLevel = C.Debug
                                           , C.logSource = C.EventHandler "ListBlocks"
                                           , C.logText = [Fmt.fmt ("Pushing a block to view: " +| C.blockAddress b ||+"")]
                                           })
                C.sEmitEvent s0 (C.PushContext archNonce fh C.BaseRepr b)
                C.sEmitEvent s0 (C.ViewBlock archNonce C.BaseRepr)
          let s1 = s0 & C.lUIMode .~ C.SomeUIMode C.BlockSelector
                      & C.lArchState . _Just . SBE.blockSelectorL .~ BS.blockSelector callback focusedListAttr blocks
          B.continue $! C.State s1
      | otherwise -> B.continue (C.State s0)

    C.FindFunctionsContaining archNonce maddr
      | Just oldArchState <- s0 ^. C.lArchState
      , Just PC.Refl <- PC.testEquality (s0 ^. C.lNonce) archNonce ->
        let ares = oldArchState ^. C.lAnalysisResult
        in case maddr of
            Nothing -> do
              liftIO (C.sEmitEvent s0 (C.ListFunctions archNonce (C.functions ares)))
              B.continue $! C.State s0
      | otherwise -> B.continue $! C.State s0

    C.ListFunctions archNonce funcs
      | Just archState <- s0 ^. C.lArchState
      , Just PC.Refl <- PC.testEquality (s0 ^. C.lNonce) archNonce -> do
          let callback f = do
                case C.functionBlocks (archState ^. C.lAnalysisResult) f of
                  [] ->
                    C.logMessage s0 (C.msgWith { C.logLevel = C.Warn
                                               , C.logSource = C.EventHandler "ListFunctions"
                                               , C.logText = [Fmt.fmt ("Failed to find blocks for function: " +| f ||+"")]
                                               })
                  entryBlock : _ -> do
                    C.logMessage s0 (C.msgWith { C.logLevel = C.Debug
                                               , C.logSource = C.EventHandler "ListFunctions"
                                               , C.logText = [Fmt.fmt ("Selecting function: " +| f ||+ "")]
                                               })
                    C.sEmitEvent s0 (C.PushContext archNonce f C.BaseRepr entryBlock)
                    C.sEmitEvent s0 (C.ViewFunction archNonce C.BaseRepr)
          let s1 = s0 & C.lUIMode .~ C.SomeUIMode C.FunctionSelector
                      & C.lArchState . _Just . SBE.functionSelectorL .~ FS.functionSelector callback focusedListAttr funcs
          B.continue (C.State s1)
      | otherwise -> B.continue (C.State s0)

    C.PushContext archNonce fh irrepr b
      | Just archState <- s0 ^. C.lArchState
      , Just PC.Refl <- PC.testEquality (s0 ^. C.lNonce) archNonce -> do
          let ng = C.sNonceGenerator s0
          (ctx, sessionState) <- liftIO $ C.makeContext ng (archState ^. C.irCacheL) (archState ^. C.lAnalysisResult) fh irrepr b
          let s1 = s0 & C.lArchState . _Just . C.contextL %~ C.pushContext ctx
                      & C.lArchState . _Just . C.symExStateL %~ (<> sessionState)
          liftIO $ C.logMessage s0 (C.msgWith { C.logLevel = C.Debug
                                              , C.logSource = C.EventHandler "PushContext"
                                              , C.logText = [ Fmt.fmt ("Selecting block: " +| C.blockAddress b ||+ "")
                                                            , Fmt.fmt ("from function " +| C.blockFunction b ||+ "")
                                                            ]})
          B.continue (C.State s1)
      | otherwise -> do
        case s0 ^. C.lArchState of
          Nothing -> liftIO $ C.logMessage s0 (C.msgWith { C.logText = ["No arch state"]
                                                         , C.logLevel = C.Warn
                                                         , C.logSource = C.EventHandler "PushContext"
                                                         })
          Just _archState
            | Just PC.Refl <- PC.testEquality (s0 ^. C.lNonce) archNonce ->
              return ()
            | otherwise ->
              liftIO $ C.logMessage s0 (C.msgWith { C.logText = ["Nonce mismatch"]
                                                  , C.logLevel = C.Warn
                                                  , C.logSource = C.EventHandler "PushContext"
                                                  })
        B.continue (C.State s0)
    C.ContextBack -> do
      let s1 = s0 & C.lArchState . _Just . C.contextL %~ C.contextBack
      B.continue $! C.State s1
    C.ContextForward -> do
      let s1 = s0 & C.lArchState . _Just . C.contextL %~ C.contextForward
      B.continue $! C.State s1

withBlockViewer :: (C.Architecture arch s)
                => C.S SBE.BrickUIExtension SBE.BrickUIState arch s
                -> (forall arch1 ir1 . PN.Nonce s arch1 -> C.IRRepr arch1 ir1 -> B.EventM n (B.Next (C.State SBE.BrickUIExtension SBE.BrickUIState s)))
                -> B.EventM n (B.Next (C.State SBE.BrickUIExtension SBE.BrickUIState s))
withBlockViewer s0 k =
  case s0 ^. C.lUIMode of
    C.SomeUIMode (C.BlockViewer vnonce repr) -> k vnonce repr
    C.SomeMiniBuffer (C.MiniBuffer m) ->
      case m of
        C.BlockViewer vnonce repr -> k vnonce repr
        _ -> B.continue $! C.State s0
    _ -> B.continue $! C.State s0
