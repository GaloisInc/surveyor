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
import           Data.Parameterized.Classes
import qualified Data.Parameterized.List as PL
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Graphics.Vty as V
import           Text.Printf ( printf )

import qualified Brick.Command as C
import qualified Brick.Keymap as K
import           Surveyor.BinaryAnalysisResult
import qualified Surveyor.EchoArea as EA
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
handleVtyEvent s0@(State s) evt
  | V.EvKey k mods <- evt
  , Just (Some cmd) <- K.lookupKeyCommand (sUIMode s) (K.Key k mods) (sKeymap s)
  , Just Refl <- testEquality (C.cmdArgTypes cmd) PL.Nil = do
      -- First, we try to consult the keymap.  For now, we can only handle
      -- commands that take no arguments.  Later, once we develop a notion of
      -- "current context", we can use that to call commands that take an
      -- argument.
      liftIO (C.cmdFunc cmd PL.Nil)
      B.continue (State s)
  | otherwise =
  case sUIMode s of
    SomeMiniBuffer (MiniBuffer oldMode) -> do
      mbs <- MB.handleMinibufferEvent evt (sMinibuffer s)
      case mbs of
        MB.Canceled mb' ->
          B.continue $ State s { sMinibuffer = mb'
                               , sUIMode = SomeUIMode oldMode
                               }
        MB.Completed mb' ->
          B.continue $ State s { sMinibuffer = mb' }
    SomeUIMode m ->
      case evt of
        V.EvKey (V.KChar 'x') [V.MMeta] ->
          -- Activate the minibuffer; due to the representation of SomeUIMode,
          -- it cannot contain a MiniBuffer (i.e., the minibuffer cannot
          -- possibly already be active if we are here), so we don't need to
          -- check for recursive minibuffer activation.
          B.continue $ State s { sUIMode = SomeMiniBuffer (MiniBuffer m) }
        V.EvKey (V.KChar 's') [] -> do
          liftIO (sEmitEvent s ShowSummary)
          B.continue (State s)
        V.EvKey (V.KChar 'm') [] -> do
          liftIO (sEmitEvent s ShowDiagnostics)
          B.continue (State s)
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
    DescribeCommand (Some cmd) -> do
      let msg = T.pack (printf "%s: %s" (C.cmdName cmd) (C.cmdDocstring cmd))
      liftIO (sEmitEvent s0 (EchoText msg))
      let newMode =
            case sUIMode s0 of
              SomeMiniBuffer (MiniBuffer oldMode) -> oldMode
              SomeUIMode mode -> mode
      B.continue $ State s0 { sDiagnosticLog = sDiagnosticLog s0 Seq.|> msg
                            , sUIMode = SomeUIMode newMode
                            }
    EchoText txt -> do
      ea' <- liftIO (EA.setText (sEchoArea s0) txt)
      B.continue $ State s0 { sEchoArea = ea' }
    UpdateEchoArea ea -> B.continue $ State s0 { sEchoArea = ea }
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
