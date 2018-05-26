{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Surveyor.Handlers (
  appHandleEvent
  ) where

import qualified Brick as B
import           Control.Lens ( (&), (^.), (.~), (%~), (^?), _Just )
import           Control.Monad.IO.Class ( liftIO )
import qualified Data.Foldable as F
import           Data.Maybe ( listToMaybe )
import           Data.Monoid
import           Data.Parameterized.Classes
import qualified Data.Parameterized.List as PL
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Graphics.Vty as V
import           Text.Printf ( printf )

import qualified Surveyor.Core.Command as C
import qualified Surveyor.Core.Keymap as K
import qualified Surveyor.Architecture as A
import           Surveyor.Attributes ( focusedListAttr )
import qualified Surveyor.Commands as C
import           Surveyor.Events
import qualified Surveyor.Keymap as K
import qualified Surveyor.Loader as SL
import qualified Surveyor.Mode as M
import           Surveyor.Names ( Names(..) )
import           Surveyor.State
import qualified Surveyor.Widget.BlockSelector as BS
import qualified Surveyor.Widget.BlockViewer as BV
import qualified Surveyor.EchoArea as EA
import qualified Surveyor.Widget.FunctionViewer as FV
import qualified Surveyor.Widget.FunctionSelector as FS
import qualified Surveyor.Widget.Minibuffer as MB

appHandleEvent :: State BrickUIState s -> B.BrickEvent Names (Events s) -> B.EventM Names (B.Next (State BrickUIState s))
appHandleEvent (State s0) evt =
  case evt of
    B.AppEvent ae -> handleCustomEvent s0 ae
    B.VtyEvent vtyEvt -> handleVtyEvent (State s0) vtyEvt
    B.MouseDown {} -> B.continue (State s0)
    B.MouseUp {} -> B.continue (State s0)

handleVtyEvent :: State BrickUIState s -> V.Event -> B.EventM Names (B.Next (State BrickUIState s))
handleVtyEvent s0@(State s) evt
  | V.EvKey k mods <- evt
  , Just km <- s ^? lArchState . _Just . lKeymap
  , Just (Some cmd) <- K.lookupKeyCommand (sUIMode s) (K.Key k mods) km
  , Just Refl <- testEquality (C.cmdArgTypes cmd) PL.Nil = do
      -- First, we try to consult the keymap.  For now, we can only handle
      -- commands that take no arguments.  Later, once we develop a notion of
      -- "current context", we can use that to call commands that take an
      -- argument.
      liftIO (C.cmdFunc cmd (sEventChannel s) (sNonce <$> sArchState s) PL.Nil)
      B.continue (State s)
  | otherwise =
  case sUIMode s of
    M.SomeMiniBuffer (M.MiniBuffer oldMode)
      | Just mb <- s ^? lArchState . _Just . lMinibuffer -> do
          mbs <- MB.handleMinibufferEvent evt (sEventChannel s) (sNonce <$> sArchState s) mb
          case mbs of
            MB.Canceled mb' -> do
              let s' = s & lUIMode .~ M.SomeUIMode oldMode
                         & lArchState . _Just . lMinibuffer .~ mb'
              B.continue $! State s'
            MB.Completed mb' -> do
              let s' = s & lArchState . _Just . lMinibuffer .~ mb'
              B.continue $! State s'
      | otherwise -> B.continue s0
    M.SomeUIMode M.BlockSelector
      | Just bsel <- s ^? lArchState . _Just . lBlockSelector -> do
          bsel' <- BS.handleBlockSelectorEvent evt bsel
          let s' = s & lArchState . _Just . lBlockSelector .~ bsel'
          B.continue $! State s'
      | otherwise -> B.continue s0
    M.SomeUIMode M.BlockViewer
      | Just bview <- s ^? lArchState . _Just . lBlockViewer -> do
          bv' <- BV.handleBlockViewerEvent evt bview
          let s' = s & lArchState . _Just . lBlockViewer .~ bv'
          B.continue $! (State s')
      | otherwise -> B.continue s0
    M.SomeUIMode M.FunctionSelector
      | Just fsel <- s ^? lArchState . _Just . lFunctionSelector -> do
          fsel' <- FS.handleFunctionSelectorEvent evt fsel
          let s' = s & lArchState . _Just . lFunctionSelector .~ fsel'
          B.continue $! State s'
      | otherwise -> B.continue s0
    M.SomeUIMode _m -> B.continue s0

handleCustomEvent :: (A.Architecture arch s) => S BrickUIState arch s -> Events s -> B.EventM Names (B.Next (State BrickUIState s))
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
    ErrorLoadingELFHeader off msg -> do
      let t = T.pack (printf "ELF Loading error at offset 0x%x: %s" off msg)
      let s1 = s0 & lDiagnosticLog %~ (Seq.|> t)
      B.continue $! State s1
    ErrorLoadingELF errs -> do
      let newDiags = map (\d -> T.pack (printf "ELF Loading error: %s" (show d))) errs
      let s1 = s0 & lDiagnosticLog %~ (<> Seq.fromList newDiags)
      B.continue $! State s1
    ErrorLoadingLLVM s -> do
      let t = T.pack (printf "Error loading LLVM bitcode: %s" s)
      let s1 = s0 & lDiagnosticLog %~ (Seq.|> t)
      B.continue $! State s1
    LoadFile filename -> do
      liftIO (F.traverse_ SL.cancelLoader (sLoader s0))
      loader <- liftIO $ SL.asynchronouslyLoad (sNonceGenerator s0) (sEventChannel s0) filename
      let s1 = s0 & lLoader .~ Just loader
                  & lInputFile .~ Just filename
      B.continue $! State s1
    LoadELF filename -> do
      liftIO (F.traverse_ SL.cancelLoader (sLoader s0))
      loader <- liftIO $ SL.asynchronouslyLoadElf (sNonceGenerator s0) (sEventChannel s0) filename
      let s1 = s0 & lLoader .~ Just loader
                  & lInputFile .~ Just filename
      B.continue $! State s1
    LoadJAR filename -> do
      liftIO (F.traverse_ SL.cancelLoader (sLoader s0))
      loader <- liftIO $ SL.asynchronouslyLoadJAR (sNonceGenerator s0) (sEventChannel s0) filename
      let s1 = s0 & lLoader .~ Just loader
                  & lInputFile .~ Just filename
      B.continue $! State s1
    LoadLLVM filename -> do
      liftIO (F.traverse_ SL.cancelLoader (sLoader s0))
      loader <- liftIO $ SL.asynchronouslyLoadLLVM (sNonceGenerator s0) (sEventChannel s0) filename
      let s1 = s0 & lLoader .~ Just loader
                  & lInputFile .~ Just filename
      B.continue $! State s1
    ShowSummary -> B.continue $! State (s0 & lUIMode .~ M.SomeUIMode M.Summary)
    ShowDiagnostics -> B.continue $! State (s0 & lUIMode .~ M.SomeUIMode M.Diags)
    DescribeCommand (Some cmd) -> do
      let msg = T.pack (printf "%s: %s" (C.cmdName cmd) (C.cmdDocstring cmd))
      liftIO (sEmitEvent s0 (EchoText msg))
      let newMode =
            case sUIMode s0 of
              M.SomeMiniBuffer (M.MiniBuffer oldMode) -> oldMode
              M.SomeUIMode mode -> mode
      let s1 = s0 & lDiagnosticLog %~ (Seq.|> msg)
                  & lUIMode .~ M.SomeUIMode newMode
      B.continue $! State s1
    EchoText txt -> do
      ea' <- liftIO (EA.setText (sEchoArea s0) txt)
      B.continue $! State (s0 & lEchoArea .~ ea')
    ResetEchoArea -> B.continue $! State (s0 & lEchoArea %~ EA.resetEchoArea)
    OpenMinibuffer ->
      case sUIMode s0 of
        M.SomeMiniBuffer _ -> B.continue (State s0)
        M.SomeUIMode mode -> B.continue $! State (s0 & lUIMode .~ M.SomeMiniBuffer (M.MiniBuffer mode))

    ViewBlock archNonce b
      | Just oldNonce <- s0 ^? lArchState . _Just . lNonce
      , Just Refl <- testEquality archNonce oldNonce -> do
          let s1 = s0 & lUIMode .~ M.SomeUIMode M.BlockViewer
                      & lArchState . _Just . lBlockViewer .~ BV.blockViewer (BlockViewerList 0) b
          B.continue $! State s1
      | otherwise -> B.continue (State s0)
    ViewFunction archNonce fh
      | Just archState <- s0 ^. lArchState
      , ares <- archState ^. lAnalysisResult
      , Just Refl <- testEquality archNonce (archState ^. lNonce) -> do
          let s1 = s0 & lUIMode .~ M.SomeUIMode M.FunctionViewer
                      & lArchState . _Just . lFunctionViewer .~ FV.functionViewer fh ares
          B.continue $! State s1
      | otherwise -> B.continue (State s0)
    FindBlockContaining archNonce addr
      | Just archState <- s0 ^. lArchState
      , ares <- archState ^. lAnalysisResult
      , Just Refl <- testEquality (archState ^. lNonce) archNonce -> do
          liftIO (sEmitEvent s0 (LogDiagnostic (T.pack (printf "Finding block at address %s" (A.prettyAddress addr)))))
          case A.containingBlocks ares addr of
            [b] -> do
              liftIO (sEmitEvent s0 (ViewBlock archNonce b))
              B.continue (State s0)
            blocks -> do
              liftIO (sEmitEvent s0 (ListBlocks archNonce blocks))
              B.continue (State s0)
      | otherwise -> B.continue (State s0)
    ListBlocks archNonce blocks
      | Just oldNonce <- s0 ^? lArchState . _Just . lNonce
      , Just Refl <- testEquality oldNonce archNonce -> do
          let callback b = sEmitEvent s0 (ViewBlock archNonce b)
          let s1 = s0 & lUIMode .~ M.SomeUIMode M.BlockSelector
                      & lArchState . _Just . lBlockSelector .~ BS.blockSelector callback focusedListAttr blocks
          B.continue $! State s1
      | otherwise -> B.continue (State s0)

    FindFunctionsContaining archNonce maddr
      | Just oldArchState <- s0 ^. lArchState
      , oldNonce <- oldArchState ^. lNonce
      , Just Refl <- testEquality oldNonce archNonce ->
        let ares = oldArchState ^. lAnalysisResult
        in case maddr of
            Nothing -> do
              liftIO (sEmitEvent s0 (ListFunctions archNonce (A.functions ares)))
              B.continue $! State s0
      | otherwise -> B.continue $! State s0

    ListFunctions archNonce funcs
      | Just oldNonce <- s0 ^? lArchState . _Just . lNonce
      , Just Refl <- testEquality oldNonce archNonce -> do
          let callback f = sEmitEvent s0 (ViewFunction archNonce f)
          let s1 = s0 & lUIMode .~ M.SomeUIMode M.FunctionSelector
                      & lArchState . _Just . lFunctionSelector .~ FS.functionSelector callback focusedListAttr funcs
          B.continue (State s1)
      | otherwise -> B.continue (State s0)

    LogDiagnostic t ->
      B.continue $! State (s0 & lDiagnosticLog %~ (Seq.|> t))
    Exit -> do
      liftIO (F.traverse_ SL.cancelLoader (sLoader s0))
      B.halt (State s0)

stateFromAnalysisResult :: (A.Architecture arch s)
                        => S BrickUIState arch0 s
                        -> A.AnalysisResult arch s
                        -> Seq.Seq T.Text
                        -> AppState
                        -> M.SomeUIMode
                        -> S BrickUIState arch s
stateFromAnalysisResult s0 ares newDiags state uiMode =
  S { sDiagnosticLog = sDiagnosticLog s0 <> newDiags
    , sUIMode = uiMode
    , sAppState = state
    , sEmitEvent = sEmitEvent s0
    , sEventChannel = sEventChannel s0
    , sNonceGenerator = sNonceGenerator s0
    , sEchoArea = sEchoArea s0
    , sInputFile = sInputFile s0
    , sLoader = sLoader s0
    , sArchState =
      case () of
        () | Just oldArchState <- sArchState s0
           , Just Refl <- testEquality (sNonce oldArchState) (A.archNonce ares) ->
             Just (oldArchState { sAnalysisResult = ares })
           | otherwise -> do
               defFunc <- listToMaybe (A.functions ares)
               b0 <- listToMaybe (A.functionBlocks ares defFunc)
               let uiState = BrickUIState { sBlockSelector = BS.emptyBlockSelector
                                          , sBlockViewer = BV.blockViewer (BlockViewerList 0) b0
                                          , sFunctionViewer = FV.functionViewer defFunc ares
                                          , sMinibuffer = MB.minibuffer MinibufferEditor MinibufferCompletionList "M-x" C.allCommands
                                          , sFunctionSelector = FS.functionSelector (const (return ())) focusedListAttr []
                                          }
               return ArchState { sAnalysisResult = ares
                                , sKeymap = K.defaultKeymap
                                , sNonce = A.archNonce ares
                                , sUIState = uiState
                                }
    }
