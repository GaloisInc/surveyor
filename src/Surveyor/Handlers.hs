{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Surveyor.Handlers (
  appHandleEvent
  ) where

import qualified Brick as B
import           Control.Monad.IO.Class ( liftIO )
import           Data.Monoid
import           Data.Parameterized.Classes
import qualified Data.Parameterized.List as PL
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Graphics.Vty as V
import           Text.Printf ( printf )

import qualified Brick.Command as C
import qualified Brick.Keymap as K
import qualified Surveyor.Architecture as A
import           Surveyor.Attributes ( focusedListAttr )
import qualified Surveyor.BlockSelector as BS
import qualified Surveyor.BlockViewer as BV
import qualified Surveyor.Commands as C
import qualified Surveyor.EchoArea as EA
import           Surveyor.Events
import qualified Surveyor.Keymap as K
import qualified Surveyor.Minibuffer as MB
import           Surveyor.Mode
import           Surveyor.Names ( Names(..) )
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
      liftIO (C.cmdFunc cmd s PL.Nil)
      B.continue (State s)
  | otherwise =
  case sUIMode s of
    SomeMiniBuffer (MiniBuffer oldMode) -> do
      mbs <- MB.handleMinibufferEvent evt s (sMinibuffer s)
      case mbs of
        MB.Canceled mb' ->
          B.continue $! State s { sMinibuffer = mb'
                               , sUIMode = SomeUIMode oldMode
                               }
        MB.Completed mb' ->
          B.continue $! State s { sMinibuffer = mb' }
    SomeUIMode BlockSelector -> do
      bsel' <- BS.handleBlockSelectorEvent evt (sBlockSelector s)
      B.continue $! State s { sBlockSelector = bsel' }
    SomeUIMode _m -> B.continue s0

handleCustomEvent :: (A.Architecture arch s) => S arch s -> Events s -> B.EventM Names (B.Next (State s))
handleCustomEvent s0 evt =
  case evt of
    AnalysisFinished (A.SomeResult bar) diags ->
      let newDiags = map (\d -> T.pack ("Analysis: " ++ show d)) diags
          notification = "Finished loading file"
          s1 = stateFromAnalysisResult s0 bar (Seq.fromList newDiags <> Seq.singleton notification) Ready (SomeUIMode Diags)
      in B.continue (State s1)
    AnalysisProgress (A.SomeResult bar) ->
      let s1 = stateFromAnalysisResult s0 bar Seq.empty Loading (sUIMode s0)
      in B.continue (State s1)
    AnalysisFailure exn ->
      B.continue $! State s0 { sDiagnosticLog = sDiagnosticLog s0 Seq.|> T.pack ("Analysis failure: " ++ show exn) }
    ErrorLoadingELFHeader off msg ->
      let t = T.pack (printf "ELF Loading error at offset 0x%x: %s" off msg)
      in B.continue $! State s0 { sDiagnosticLog = sDiagnosticLog s0 Seq.|> t }
    ErrorLoadingELF errs ->
      let newDiags = map (\d -> T.pack (printf "ELF Loading error: %s" (show d))) errs
      in B.continue $! State s0 { sDiagnosticLog = sDiagnosticLog s0 <> Seq.fromList newDiags }
    ShowSummary -> B.continue $! State s0 { sUIMode = SomeUIMode Summary }
    ShowDiagnostics -> B.continue $! State s0 { sUIMode = SomeUIMode Diags }
    DescribeCommand (Some cmd) -> do
      let msg = T.pack (printf "%s: %s" (C.cmdName cmd) (C.cmdDocstring cmd))
      liftIO (sEmitEvent s0 (EchoText msg))
      let newMode =
            case sUIMode s0 of
              SomeMiniBuffer (MiniBuffer oldMode) -> oldMode
              SomeUIMode mode -> mode
      B.continue $! State s0 { sDiagnosticLog = sDiagnosticLog s0 Seq.|> msg
                            , sUIMode = SomeUIMode newMode
                            }
    EchoText txt -> do
      ea' <- liftIO (EA.setText (sEchoArea s0) txt)
      B.continue $! State s0 { sEchoArea = ea' }
    UpdateEchoArea ea -> B.continue $! State s0 { sEchoArea = ea }
    OpenMinibuffer ->
      case sUIMode s0 of
        SomeMiniBuffer _ -> B.continue (State s0)
        SomeUIMode mode -> B.continue $! State s0 { sUIMode = SomeMiniBuffer (MiniBuffer mode) }

    ViewBlock archNonce b
      | Just Refl <- testEquality archNonce (sArch s0) ->
        B.continue $! State s0 { sUIMode = SomeUIMode BlockViewer
                              , sBlockViewer = BV.blockViewer b
                              }
      | otherwise -> B.continue (State s0)
    FindBlockContaining archNonce addr ->
      case sAnalysisResult s0 of
        Just ares
          | Just Refl <- testEquality (sArch s0) archNonce -> do
              case A.containingBlocks ares addr of
                [b] -> do
                  liftIO (sEmitEvent s0 (ViewBlock archNonce b))
                  B.continue (State s0)
                blocks -> do
                  liftIO (sEmitEvent s0 (ListBlocks archNonce blocks))
                  B.continue (State s0)
        _ -> B.continue (State s0)
    ListBlocks archNonce blocks
      | Just Refl <- testEquality (sArch s0) archNonce -> do
          let callback b = sEmitEvent s0 (ViewBlock archNonce b)
          B.continue $! State s0 { sBlockSelector = BS.blockSelector callback focusedListAttr blocks
                                , sUIMode = SomeUIMode BlockSelector
                                }
       | otherwise -> B.continue (State s0)
    Exit -> B.halt (State s0)

stateFromAnalysisResult :: (A.Architecture arch s)
                        => S arch0 s
                        -> A.AnalysisResult arch s
                        -> Seq.Seq T.Text
                        -> AppState
                        -> SomeUIMode
                        -> S arch s
stateFromAnalysisResult s0 ares newDiags state uiMode =
  S { sAnalysisResult = Just ares
--    , sFunctionList = B.list FunctionList funcList 1
    , sDiagnosticLog = sDiagnosticLog s0 <> newDiags
    , sEchoArea = sEchoArea s0
    , sUIMode = uiMode
    , sInputFile = sInputFile s0
    , sBlockViewer =
      case testEquality (sArch s0) (A.archNonce ares) of
        Nothing -> BV.emptyBlockViewer
        Just Refl -> sBlockViewer s0
    , sBlockSelector =
      case testEquality (sArch s0) (A.archNonce ares) of
        Just Refl -> sBlockSelector s0
        Nothing -> BS.emptyBlockSelector
    , sMinibuffer =
      case testEquality (sArch s0) (A.archNonce ares) of
        Just Refl -> sMinibuffer s0
        Nothing -> MB.minibuffer MinibufferEditor MinibufferCompletionList "M-x" (C.allCommands (sEventChannel s0))
    , sAppState = state
    , sEmitEvent = sEmitEvent s0
    , sEventChannel = sEventChannel s0
    , sNonceGenerator = sNonceGenerator s0
    , sKeymap =
      case testEquality (sArch s0) (A.archNonce ares) of
        Just Refl -> sKeymap s0
        Nothing -> K.defaultKeymap (sEventChannel s0)
    , sArch = A.archNonce ares
    }
  -- where
  --   funcList = V.fromList [ FLE addr textName blockCount
  --                         | (addr, Some dfi) <- M.toList (R.biDiscoveryFunInfo (rBlockInfo bar))
  --                         , let textName = TE.decodeUtf8With TE.lenientDecode (MD.discoveredFunName dfi)
  --                         , let blockCount = M.size (dfi L.^. MD.parsedBlocks)
  --                         ]
