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
import qualified Surveyor.Commands as C
import           Surveyor.Events
import qualified Surveyor.Keymap as K
import qualified Surveyor.Mode as M
import           Surveyor.Names ( Names(..) )
import           Surveyor.State
import qualified Surveyor.Widget.BlockSelector as BS
import qualified Surveyor.Widget.BlockViewer as BV
import qualified Surveyor.Widget.EchoArea as EA
import qualified Surveyor.Widget.FunctionSelector as FS
import qualified Surveyor.Widget.Minibuffer as MB

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
  , Just (Some cmd) <- K.lookupKeyCommand (sUIMode s) (K.Key k mods) (sKeymap (sArchState s))
  , Just Refl <- testEquality (C.cmdArgTypes cmd) PL.Nil = do
      -- First, we try to consult the keymap.  For now, we can only handle
      -- commands that take no arguments.  Later, once we develop a notion of
      -- "current context", we can use that to call commands that take an
      -- argument.
      liftIO (C.cmdFunc cmd s PL.Nil)
      B.continue (State s)
  | otherwise =
  case sUIMode s of
    M.SomeMiniBuffer (M.MiniBuffer oldMode) -> do
      mbs <- MB.handleMinibufferEvent evt s (sMinibuffer (sArchState s))
      case mbs of
        MB.Canceled mb' ->
          B.continue $! State s { sUIMode = M.SomeUIMode oldMode
                                , sArchState = (sArchState s) { sMinibuffer = mb' }
                                }
        MB.Completed mb' ->
          B.continue $! State s { sArchState = (sArchState s) { sMinibuffer = mb' } }
    M.SomeUIMode M.BlockSelector -> do
      bsel' <- BS.handleBlockSelectorEvent evt (sBlockSelector (sArchState s))
      B.continue $! State s { sArchState = (sArchState s) { sBlockSelector = bsel' } }
    M.SomeUIMode M.FunctionSelector -> do
      fsel' <- FS.handleFunctionSelectorEvent evt (sFunctionSelector (sArchState s))
      B.continue $! State s { sArchState = (sArchState s) { sFunctionSelector = fsel' } }
    M.SomeUIMode _m -> B.continue s0

handleCustomEvent :: (A.Architecture arch s) => S arch s -> Events s -> B.EventM Names (B.Next (State s))
handleCustomEvent s0 evt =
  case evt of
    AnalysisFinished (A.SomeResult bar) diags ->
      let newDiags = map (\d -> T.pack ("Analysis: " ++ show d)) diags
          notification = "Finished loading file"
          s1 = stateFromAnalysisResult s0 bar (Seq.fromList newDiags <> Seq.singleton notification) Ready (M.SomeUIMode M.Diags)
      in B.continue (State s1)
    AnalysisProgress (A.SomeResult bar) ->
      let s1 = stateFromAnalysisResult s0 bar Seq.empty Loading (sUIMode s0)
      in B.continue (State s1)
    AnalysisFailure exn -> do
      liftIO (sEmitEvent s0 (LogDiagnostic (T.pack ("Analysis failure: " ++ show exn))))
      B.continue $! State s0
    ErrorLoadingELFHeader off msg ->
      let t = T.pack (printf "ELF Loading error at offset 0x%x: %s" off msg)
      in B.continue $! State s0 { sDiagnosticLog = sDiagnosticLog s0 Seq.|> t }
    ErrorLoadingELF errs ->
      let newDiags = map (\d -> T.pack (printf "ELF Loading error: %s" (show d))) errs
      in B.continue $! State s0 { sDiagnosticLog = sDiagnosticLog s0 <> Seq.fromList newDiags }
    ShowSummary -> B.continue $! State s0 { sUIMode = M.SomeUIMode M.Summary }
    ShowDiagnostics -> B.continue $! State s0 { sUIMode = M.SomeUIMode M.Diags }
    DescribeCommand (Some cmd) -> do
      let msg = T.pack (printf "%s: %s" (C.cmdName cmd) (C.cmdDocstring cmd))
      liftIO (sEmitEvent s0 (EchoText msg))
      let newMode =
            case sUIMode s0 of
              M.SomeMiniBuffer (M.MiniBuffer oldMode) -> oldMode
              M.SomeUIMode mode -> mode
      B.continue $! State s0 { sDiagnosticLog = sDiagnosticLog s0 Seq.|> msg
                            , sUIMode = M.SomeUIMode newMode
                            }
    EchoText txt -> do
      ea' <- liftIO (EA.setText (sEchoArea s0) txt)
      B.continue $! State s0 { sEchoArea = ea' }
    UpdateEchoArea ea -> B.continue $! State s0 { sEchoArea = ea }
    OpenMinibuffer ->
      case sUIMode s0 of
        M.SomeMiniBuffer _ -> B.continue (State s0)
        M.SomeUIMode mode -> B.continue $! State s0 { sUIMode = M.SomeMiniBuffer (M.MiniBuffer mode) }

    ViewBlock archNonce b
      | Just Refl <- testEquality archNonce (sNonce (sArchState s0)) ->
        B.continue $! State s0 { sUIMode = M.SomeUIMode M.BlockViewer
                               , sArchState = (sArchState s0) { sBlockViewer = BV.blockViewer b }
                              }
      | otherwise -> B.continue (State s0)
    FindBlockContaining archNonce addr ->
      case sAnalysisResult (sArchState s0) of
        Just ares
          | Just Refl <- testEquality (sNonce (sArchState s0)) archNonce -> do
              liftIO (sEmitEvent s0 (LogDiagnostic (T.pack (printf "Finding block at address %s" (A.prettyAddress addr)))))
              case A.containingBlocks ares addr of
                [b] -> do
                  liftIO (sEmitEvent s0 (ViewBlock archNonce b))
                  B.continue (State s0)
                blocks -> do
                  liftIO (sEmitEvent s0 (ListBlocks archNonce blocks))
                  B.continue (State s0)
        _ -> B.continue (State s0)
    ListBlocks archNonce blocks
      | Just Refl <- testEquality (sNonce (sArchState s0)) archNonce -> do
          let callback b = sEmitEvent s0 (ViewBlock archNonce b)
          B.continue $! State s0 { sUIMode = M.SomeUIMode M.BlockSelector
                                 , sArchState = (sArchState s0) { sBlockSelector = BS.blockSelector callback focusedListAttr blocks
                                                                }
                                 }
       | otherwise -> B.continue (State s0)

    FindFunctionsContaining archNonce maddr
      | Just Refl <- testEquality (sNonce (sArchState s0)) archNonce ->
        case sAnalysisResult (sArchState s0) of
          Nothing -> B.continue $! State s0
          Just ares ->
            case maddr of
              Nothing -> do
                liftIO (sEmitEvent s0 (ListFunctions archNonce (A.functions ares)))
                B.continue $! State s0
      | otherwise -> B.continue $! State s0

    ListFunctions archNonce funcs
      | Just Refl <- testEquality (sNonce (sArchState s0)) archNonce -> do
          let callback f = sEmitEvent s0 (ViewFunction archNonce f)
          B.continue $! State s0 { sUIMode = M.SomeUIMode M.FunctionSelector
                                 , sArchState = (sArchState s0) { sFunctionSelector = FS.functionSelector callback focusedListAttr funcs
                                                                }
                                 }
      | otherwise -> B.continue (State s0)

    LogDiagnostic t ->
      B.continue $! State s0 { sDiagnosticLog = sDiagnosticLog s0 Seq.|> t }
    Exit -> B.halt (State s0)

stateFromAnalysisResult :: (A.Architecture arch s)
                        => S arch0 s
                        -> A.AnalysisResult arch s
                        -> Seq.Seq T.Text
                        -> AppState
                        -> M.SomeUIMode
                        -> S arch s
stateFromAnalysisResult s0 ares newDiags state uiMode =
  S { sDiagnosticLog = sDiagnosticLog s0 <> newDiags
    , sUIMode = uiMode
    , sAppState = state
    , sEmitEvent = sEmitEvent s0
    , sEventChannel = sEventChannel s0
    , sNonceGenerator = sNonceGenerator s0
    , sEchoArea = sEchoArea s0
    , sInputFile = sInputFile s0
    , sArchState =
      case testEquality (A.archNonce ares) (sNonce (sArchState s0)) of
        Just Refl -> (sArchState s0) { sAnalysisResult = Just ares }
        Nothing ->
          ArchState { sAnalysisResult = Just ares
                    , sBlockSelector = BS.emptyBlockSelector
                    , sBlockViewer = BV.emptyBlockViewer
                    , sMinibuffer = MB.minibuffer MinibufferEditor MinibufferCompletionList "M-x" (C.allCommands (sEventChannel s0))
                    , sFunctionSelector = FS.functionSelector (const (return ())) focusedListAttr []
                    , sKeymap = K.defaultKeymap (sEventChannel s0)
                    , sNonce = A.archNonce ares
                    }
    }
