{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Surveyor.Handlers (
  appHandleEvent
  ) where

import qualified Brick as B
import qualified Brick.Widgets.List as B
import           Control.Monad.IO.Class ( liftIO )
import qualified Data.Macaw.Memory as MM
import           Data.Monoid
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Graphics.Vty as V
import           Text.Printf ( printf )

import           Surveyor.BinaryAnalysisResult
import           Surveyor.Events
import qualified Surveyor.Minibuffer as MB
import           Surveyor.Mode
import           Surveyor.State

appHandleEvent :: State s -> B.BrickEvent Names (Events s) -> B.EventM Names (B.Next (State s))
appHandleEvent (State s0) evt =
  case evt of
    B.AppEvent ae -> handleCustomEvent s0 ae
    B.VtyEvent vtyEvt -> handleVtyEvent (State s0) vtyEvt
    B.MouseDown {} -> B.continue (State s0)
    B.MouseUp {} -> B.continue (State s0)

handleVtyEvent :: State s -> V.Event -> B.EventM Names (B.Next (State s))
handleVtyEvent s0@(State s) evt =
  case sUIMode s of
    SomeMiniBuffer (MiniBuffer oldMode) ->
      case evt of
        V.EvKey (V.KChar 'q') [V.MCtrl] -> do
          liftIO (sEmitEvent s Exit)
          B.continue (State s)
        _ -> do
          mbs <- MB.handleMinibufferEvent evt (sMinibuffer s)
          case mbs of
            MB.Canceled mb' ->
              B.continue $ State s { sMinibuffer = mb'
                                   , sUIMode = SomeUIMode oldMode
                                   }
            MB.Completed mb' ->
              B.continue $ State s { sMinibuffer = mb' }
    SomeUIMode _ ->
      case evt of
        V.EvKey (V.KChar 'x') [V.MMeta] ->
          case sUIMode s of
            SomeMiniBuffer (MiniBuffer _) -> B.continue s0
            SomeUIMode oldMode -> B.continue $ State (s { sUIMode = SomeMiniBuffer (MiniBuffer oldMode) })
        V.EvKey (V.KChar 's') [] -> do
          liftIO (sEmitEvent s ShowSummary)
          B.continue (State s)
        V.EvKey (V.KChar 'm') [] -> do
          liftIO (sEmitEvent s ShowDiagnostics)
          B.continue (State s)
        V.EvKey (V.KChar 'q') [V.MCtrl] -> do
          liftIO (sEmitEvent s Exit)
          B.continue (State s)
        V.EvKey _k [] -> B.continue s0
        _ -> B.continue s0


handleCustomEvent :: (MM.MemWidth w) => S s i a w arch -> Events s -> B.EventM Names (B.Next (State s))
handleCustomEvent s0 evt =
  case evt of
    AnalysisFinished (BinaryAnalysisResultWrapper bar) diags ->
      let newDiags = map (\d -> T.pack ("Analysis: " ++ show d)) diags
          notification = "Finished loading file"
          s1 = stateFromAnalysisResult s0 bar (Seq.fromList newDiags <> Seq.singleton notification) Ready (SomeUIMode Diags)
      in B.continue (State s1)
    AnalysisProgress _addr (BinaryAnalysisResultWrapper bar) ->
      let s1 = stateFromAnalysisResult s0 bar Seq.empty Loading (sUIMode s0)
      in B.continue (State s1)
    BlockDiscovered addr ->
      B.continue $ State s0 { sDiagnosticLog = sDiagnosticLog s0 Seq.|> T.pack ("Found a block at address " ++ show addr) }
    AnalysisFailure exn ->
      B.continue $ State s0 { sDiagnosticLog = sDiagnosticLog s0 Seq.|> T.pack ("Analysis failure: " ++ show exn) }
    ErrorLoadingELFHeader off msg ->
      let t = T.pack (printf "ELF Loading error at offset 0x%x: %s" off msg)
      in B.continue $ State s0 { sDiagnosticLog = sDiagnosticLog s0 Seq.|> t }
    ErrorLoadingELF errs ->
      let newDiags = map (\d -> T.pack (printf "ELF Loading error: %s" (show d))) errs
      in B.continue $ State s0 { sDiagnosticLog = sDiagnosticLog s0 <> Seq.fromList newDiags }
    ShowSummary -> B.continue $ State s0 { sUIMode = SomeUIMode Summary }
    ShowDiagnostics -> B.continue $ State s0 { sUIMode = SomeUIMode Diags }
    FindBlockContaining addr ->
      case sBinaryInfo s0 of
        Nothing -> B.continue (State s0)
        Just bar -> do
          let absAddr = MM.absoluteAddr (fromIntegral addr)
          let blocks = blocksContaining bar absAddr
          B.continue $ State s0 { sBlockList = (absAddr, B.list BlockList (V.fromList blocks) 1)
                                , sDiagnosticLog = sDiagnosticLog s0 <> Seq.fromList [T.pack ("Finding blocks containing " ++ show absAddr)]
                                , sUIMode = SomeUIMode BlockSelector
                                }
    Exit -> B.halt (State s0)
