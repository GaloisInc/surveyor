{-# LANGUAGE GADTs #-}
module Surveyor.Brick.Handlers.Extension ( handleExtensionEvent ) where

import qualified Brick as B
import           Control.Lens ( (&), (.~), (^.), (%~), _Just )
import           Control.Monad.IO.Class ( liftIO )
import qualified Data.Parameterized.Classes as PC
import qualified Data.Text as T
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.Text as PPT
import qualified Surveyor.Core as C

import           Surveyor.Brick.Attributes ( focusedListAttr )
import qualified Surveyor.Brick.EchoArea as SBEA
import qualified Surveyor.Brick.Extension as SBE
import qualified Surveyor.Brick.Widget.BlockSelector as BS
import qualified Surveyor.Brick.Widget.FunctionSelector as FS
import qualified Surveyor.Brick.Widget.Minibuffer as MB

handleExtensionEvent :: ( C.Architecture arch s
                        , C.CrucibleExtension arch
                        )
                     => C.S SBE.BrickUIExtension SBE.BrickUIState arch s
                     -> SBE.BrickUIEvent s st
                     -> B.EventM n (B.Next (C.State SBE.BrickUIExtension SBE.BrickUIState s))
handleExtensionEvent s0 evt =
  case evt of
    SBE.ShowSummary -> B.continue $! C.State (s0 & C.lUIMode .~ C.SomeUIMode C.Summary)
    SBE.ShowDiagnostics -> B.continue $! C.State (s0 & C.lUIMode .~ C.SomeUIMode C.Diags)
    SBE.ShowSymbolicExecution -> B.continue $! C.State (s0 & C.lUIMode .~ C.SomeUIMode C.SymbolicExecutionManager)
    SBE.OpenMinibuffer ->
      case C.sUIMode s0 of
        C.SomeMiniBuffer _ -> B.continue (C.State s0)
        C.SomeUIMode mode -> B.continue $! C.State (s0 & C.lUIMode .~ C.SomeMiniBuffer (C.MiniBuffer mode))

    SBE.FindBlockContaining archNonce addr
      | Just archState <- s0 ^. C.lArchState
      , ares <- archState ^. C.lAnalysisResult
      , Just PC.Refl <- PC.testEquality (s0 ^. C.lNonce) archNonce -> do
          liftIO $ C.logMessage s0 (C.msgWith { C.logLevel = C.Debug
                                              , C.logSource = C.EventHandler (T.pack "FindBlockContaining")
                                              , C.logText = [PP.pretty "Finding block at address" PP.<+> C.prettyAddress addr]
                                              })
          case C.containingBlocks ares addr of
            [b] -> do
              let fh = C.blockFunction b
              liftIO (C.sEmitEvent s0 (C.PushContext archNonce fh C.BaseRepr b))
              liftIO (C.sEmitEvent s0 (C.ViewBlock archNonce C.BaseRepr))
              B.continue (C.State s0)
            blocks -> do
              liftIO (C.sEmitEvent s0 (SBE.ListBlocks archNonce blocks))
              B.continue (C.State s0)
      | otherwise -> B.continue (C.State s0)

    SBE.FindFunctionsContaining archNonce maddr
      | Just oldArchState <- s0 ^. C.lArchState
      , Just PC.Refl <- PC.testEquality (s0 ^. C.lNonce) archNonce ->
        let ares = oldArchState ^. C.lAnalysisResult
        in case maddr of
            Nothing -> do
              liftIO (C.sEmitEvent s0 (SBE.ListFunctions archNonce (C.functions ares)))
              B.continue $! C.State s0
      | otherwise -> B.continue $! C.State s0

    SBE.ListBlocks archNonce blocks
      | Just PC.Refl <- PC.testEquality (s0 ^. C.lNonce) archNonce -> do
          let callback b = do
                let fh = C.blockFunction b
                C.logMessage s0 (C.msgWith { C.logLevel = C.Debug
                                           , C.logSource = C.EventHandler (T.pack "ListBlocks")
                                           , C.logText = [PP.pretty "Pushing a block to view:" PP.<+> PP.viaShow (C.blockAddress b)]
                                           })
                C.sEmitEvent s0 (C.PushContext archNonce fh C.BaseRepr b)
                C.sEmitEvent s0 (C.ViewBlock archNonce C.BaseRepr)
          let s1 = s0 & C.lUIMode .~ C.SomeUIMode C.BlockSelector
                      & C.lArchState . _Just . C.lUIState . SBE.blockSelectorL .~ BS.blockSelector callback focusedListAttr blocks
          B.continue $! C.State s1
      | otherwise -> B.continue (C.State s0)

    SBE.ListFunctions archNonce funcs
      | Just archState <- s0 ^. C.lArchState
      , Just PC.Refl <- PC.testEquality (s0 ^. C.lNonce) archNonce -> do
          let callback f = do
                case C.functionBlocks (archState ^. C.lAnalysisResult) f of
                  [] ->
                    C.logMessage s0 (C.msgWith { C.logLevel = C.Warn
                                               , C.logSource = C.EventHandler (T.pack "ListFunctions")
                                               , C.logText = [PP.pretty "Failed to find blocks for function:" PP.<+> PP.viaShow f]
                                               })
                  entryBlock : _ -> do
                    C.logMessage s0 (C.msgWith { C.logLevel = C.Debug
                                               , C.logSource = C.EventHandler (T.pack "ListFunctions")
                                               , C.logText = [PP.pretty "Selecting function:" PP.<+> PP.viaShow f]
                                               })
                    C.sEmitEvent s0 (C.PushContext archNonce f C.BaseRepr entryBlock)
                    C.sEmitEvent s0 (C.ViewFunction archNonce C.BaseRepr)
          let s1 = s0 & C.lUIMode .~ C.SomeUIMode C.FunctionSelector
                      & C.lArchState . _Just . C.lUIState . SBE.functionSelectorL .~ FS.functionSelector callback focusedListAttr funcs
          B.continue (C.State s1)
      | otherwise -> B.continue (C.State s0)

    SBE.PromptValueName archNonce
      | Just PC.Refl <- PC.testEquality archNonce (s0 ^. C.lNonce)
      , C.SomeUIMode curMode <- s0 ^. C.lUIMode
      , mb <- s0 ^. C.lUIExtension . SBE.minibufferG -> do
          let chan = s0 ^. C.lEventChannel
          mbs <- liftIO $ MB.invokeCommand chan (C.SomeState s0) mb (C.SomeCommand C.nameCurrentValueC)
          case mbs of
            MB.Canceled mb' -> do
              let s1 = s0 & C.lUIExtension . SBE.minibufferL .~ mb'
              B.continue $! C.State s1
            MB.Completed mb' -> do
              let s1 = s0 & C.lUIExtension . SBE.minibufferL .~ mb'
                          & C.lUIMode .~ C.SomeMiniBuffer (C.MiniBuffer curMode)
              B.continue $! C.State s1
            MB.Executed mb' -> do
              let s1 = s0 & C.lUIExtension . SBE.minibufferL .~ mb'
              B.continue $! C.State s1
      | otherwise -> B.continue (C.State s0)
    SBE.EchoText txt -> do
      ea' <- liftIO (SBEA.setEchoAreaText (SBE.sEchoArea (C.sUIExtension s0)) (PPT.renderStrict (PP.layoutCompact txt)))
      B.continue $! C.State (s0 & C.lUIExtension . SBE.echoAreaL .~ ea')
    SBE.ResetEchoArea -> B.continue $! C.State (s0 & C.lUIExtension . SBE.echoAreaL %~ SBEA.resetEchoArea)
