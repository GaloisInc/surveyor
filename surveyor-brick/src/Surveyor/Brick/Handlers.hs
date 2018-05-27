{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Surveyor.Brick.Handlers (
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

import qualified Surveyor.Core as C
import           Surveyor.Brick.Attributes ( focusedListAttr )
import           Surveyor.Brick.Names ( Names(..) )
import           Surveyor.Brick.State
import qualified Surveyor.Brick.Widget.BlockSelector as BS
import qualified Surveyor.Brick.Widget.BlockViewer as BV
import qualified Surveyor.Brick.Widget.FunctionViewer as FV
import qualified Surveyor.Brick.Widget.FunctionSelector as FS
import qualified Surveyor.Brick.Widget.Minibuffer as MB

appHandleEvent :: State BrickUIState s -> B.BrickEvent Names (C.Events s) -> B.EventM Names (B.Next (State BrickUIState s))
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
  , Just (Some cmd) <- C.lookupKeyCommand (sUIMode s) (C.Key k mods) km
  , Just Refl <- testEquality (C.cmdArgTypes cmd) PL.Nil = do
      -- First, we try to consult the keymap.  For now, we can only handle
      -- commands that take no arguments.  Later, once we develop a notion of
      -- "current context", we can use that to call commands that take an
      -- argument.
      liftIO (C.cmdFunc cmd (sEventChannel s) (sNonce <$> sArchState s) PL.Nil)
      B.continue (State s)
  | otherwise =
  case sUIMode s of
    C.SomeMiniBuffer (C.MiniBuffer oldMode)
      | Just mb <- s ^? lArchState . _Just . lMinibuffer -> do
          mbs <- MB.handleMinibufferEvent evt (sEventChannel s) (sNonce <$> sArchState s) mb
          case mbs of
            MB.Canceled mb' -> do
              let s' = s & lUIMode .~ C.SomeUIMode oldMode
                         & lArchState . _Just . lMinibuffer .~ mb'
              B.continue $! State s'
            MB.Completed mb' -> do
              let s' = s & lArchState . _Just . lMinibuffer .~ mb'
              B.continue $! State s'
      | otherwise -> B.continue s0
    C.SomeUIMode C.BlockSelector
      | Just bsel <- s ^? lArchState . _Just . lBlockSelector -> do
          bsel' <- BS.handleBlockSelectorEvent evt bsel
          let s' = s & lArchState . _Just . lBlockSelector .~ bsel'
          B.continue $! State s'
      | otherwise -> B.continue s0
    C.SomeUIMode C.BlockViewer
      | Just bview <- s ^? lArchState . _Just . lBlockViewer -> do
          bv' <- BV.handleBlockViewerEvent evt bview
          let s' = s & lArchState . _Just . lBlockViewer .~ bv'
          B.continue $! (State s')
      | otherwise -> B.continue s0
    C.SomeUIMode C.FunctionSelector
      | Just fsel <- s ^? lArchState . _Just . lFunctionSelector -> do
          fsel' <- FS.handleFunctionSelectorEvent evt fsel
          let s' = s & lArchState . _Just . lFunctionSelector .~ fsel'
          B.continue $! State s'
      | otherwise -> B.continue s0
    C.SomeUIMode _m -> B.continue s0

handleCustomEvent :: (C.Architecture arch s) => S BrickUIState arch s -> C.Events s -> B.EventM Names (B.Next (State BrickUIState s))
handleCustomEvent s0 evt =
  case evt of
    C.AnalysisFinished (C.SomeResult bar) diags ->
      let newDiags = map (\d -> T.pack ("Analysis: " ++ show d)) diags
          notification = "Finished loading file"
          s1 = stateFromAnalysisResult s0 bar (Seq.fromList newDiags <> Seq.singleton notification) Ready (C.SomeUIMode C.Diags)
      in B.continue (State s1)
    C.AnalysisProgress (C.SomeResult bar) ->
      let s1 = stateFromAnalysisResult s0 bar Seq.empty Loading (sUIMode s0)
      in B.continue (State s1)
    C.AnalysisFailure exn -> do
      liftIO (sEmitEvent s0 (C.LogDiagnostic (T.pack ("Analysis failure: " ++ show exn))))
      B.continue $! State s0
    C.ErrorLoadingELFHeader off msg -> do
      let t = T.pack (printf "ELF Loading error at offset 0x%x: %s" off msg)
      let s1 = s0 & lDiagnosticLog %~ (Seq.|> t)
      B.continue $! State s1
    C.ErrorLoadingELF errs -> do
      let newDiags = map (\d -> T.pack (printf "ELF Loading error: %s" (show d))) errs
      let s1 = s0 & lDiagnosticLog %~ (<> Seq.fromList newDiags)
      B.continue $! State s1
    C.ErrorLoadingLLVM s -> do
      let t = T.pack (printf "Error loading LLVM bitcode: %s" s)
      let s1 = s0 & lDiagnosticLog %~ (Seq.|> t)
      B.continue $! State s1
    C.LoadFile filename -> do
      liftIO (F.traverse_ C.cancelLoader (sLoader s0))
      loader <- liftIO $ C.asynchronouslyLoad (sNonceGenerator s0) (sEventChannel s0) filename
      let s1 = s0 & lLoader .~ Just loader
                  & lInputFile .~ Just filename
      B.continue $! State s1
    C.LoadELF filename -> do
      liftIO (F.traverse_ C.cancelLoader (sLoader s0))
      loader <- liftIO $ C.asynchronouslyLoadElf (sNonceGenerator s0) (sEventChannel s0) filename
      let s1 = s0 & lLoader .~ Just loader
                  & lInputFile .~ Just filename
      B.continue $! State s1
    C.LoadJAR filename -> do
      liftIO (F.traverse_ C.cancelLoader (sLoader s0))
      loader <- liftIO $ C.asynchronouslyLoadJAR (sNonceGenerator s0) (sEventChannel s0) filename
      let s1 = s0 & lLoader .~ Just loader
                  & lInputFile .~ Just filename
      B.continue $! State s1
    C.LoadLLVM filename -> do
      liftIO (F.traverse_ C.cancelLoader (sLoader s0))
      loader <- liftIO $ C.asynchronouslyLoadLLVM (sNonceGenerator s0) (sEventChannel s0) filename
      let s1 = s0 & lLoader .~ Just loader
                  & lInputFile .~ Just filename
      B.continue $! State s1
    C.ShowSummary -> B.continue $! State (s0 & lUIMode .~ C.SomeUIMode C.Summary)
    C.ShowDiagnostics -> B.continue $! State (s0 & lUIMode .~ C.SomeUIMode C.Diags)
    C.DescribeCommand (Some cmd) -> do
      let msg = T.pack (printf "%s: %s" (C.cmdName cmd) (C.cmdDocstring cmd))
      liftIO (sEmitEvent s0 (C.EchoText msg))
      let newMode =
            case sUIMode s0 of
              C.SomeMiniBuffer (C.MiniBuffer oldMode) -> oldMode
              C.SomeUIMode mode -> mode
      let s1 = s0 & lDiagnosticLog %~ (Seq.|> msg)
                  & lUIMode .~ C.SomeUIMode newMode
      B.continue $! State s1
    C.EchoText txt -> do
      ea' <- liftIO (C.setEchoAreaText (sEchoArea s0) txt)
      B.continue $! State (s0 & lEchoArea .~ ea')
    C.ResetEchoArea -> B.continue $! State (s0 & lEchoArea %~ C.resetEchoArea)
    C.OpenMinibuffer ->
      case sUIMode s0 of
        C.SomeMiniBuffer _ -> B.continue (State s0)
        C.SomeUIMode mode -> B.continue $! State (s0 & lUIMode .~ C.SomeMiniBuffer (C.MiniBuffer mode))

    C.ViewBlock archNonce b
      | Just oldNonce <- s0 ^? lArchState . _Just . lNonce
      , Just Refl <- testEquality archNonce oldNonce -> do
          let s1 = s0 & lUIMode .~ C.SomeUIMode C.BlockViewer
                      & lArchState . _Just . lBlockViewer .~ BV.blockViewer (BlockViewerList 0) b
          B.continue $! State s1
      | otherwise -> B.continue (State s0)
    C.ViewFunction archNonce fh
      | Just archState <- s0 ^. lArchState
      , ares <- archState ^. lAnalysisResult
      , Just Refl <- testEquality archNonce (archState ^. lNonce) -> do
          let s1 = s0 & lUIMode .~ C.SomeUIMode C.FunctionViewer
                      & lArchState . _Just . lFunctionViewer .~ FV.functionViewer fh ares
          B.continue $! State s1
      | otherwise -> B.continue (State s0)
    C.FindBlockContaining archNonce addr
      | Just archState <- s0 ^. lArchState
      , ares <- archState ^. lAnalysisResult
      , Just Refl <- testEquality (archState ^. lNonce) archNonce -> do
          liftIO (sEmitEvent s0 (C.LogDiagnostic (T.pack (printf "Finding block at address %s" (C.prettyAddress addr)))))
          case C.containingBlocks ares addr of
            [b] -> do
              liftIO (sEmitEvent s0 (C.ViewBlock archNonce b))
              B.continue (State s0)
            blocks -> do
              liftIO (sEmitEvent s0 (C.ListBlocks archNonce blocks))
              B.continue (State s0)
      | otherwise -> B.continue (State s0)
    C.ListBlocks archNonce blocks
      | Just oldNonce <- s0 ^? lArchState . _Just . lNonce
      , Just Refl <- testEquality oldNonce archNonce -> do
          let callback b = sEmitEvent s0 (C.ViewBlock archNonce b)
          let s1 = s0 & lUIMode .~ C.SomeUIMode C.BlockSelector
                      & lArchState . _Just . lBlockSelector .~ BS.blockSelector callback focusedListAttr blocks
          B.continue $! State s1
      | otherwise -> B.continue (State s0)

    C.FindFunctionsContaining archNonce maddr
      | Just oldArchState <- s0 ^. lArchState
      , oldNonce <- oldArchState ^. lNonce
      , Just Refl <- testEquality oldNonce archNonce ->
        let ares = oldArchState ^. lAnalysisResult
        in case maddr of
            Nothing -> do
              liftIO (sEmitEvent s0 (C.ListFunctions archNonce (C.functions ares)))
              B.continue $! State s0
      | otherwise -> B.continue $! State s0

    C.ListFunctions archNonce funcs
      | Just oldNonce <- s0 ^? lArchState . _Just . lNonce
      , Just Refl <- testEquality oldNonce archNonce -> do
          let callback f = sEmitEvent s0 (C.ViewFunction archNonce f)
          let s1 = s0 & lUIMode .~ C.SomeUIMode C.FunctionSelector
                      & lArchState . _Just . lFunctionSelector .~ FS.functionSelector callback focusedListAttr funcs
          B.continue (State s1)
      | otherwise -> B.continue (State s0)

    C.LogDiagnostic t ->
      B.continue $! State (s0 & lDiagnosticLog %~ (Seq.|> t))
    C.Exit -> do
      liftIO (F.traverse_ C.cancelLoader (sLoader s0))
      B.halt (State s0)

stateFromAnalysisResult :: (C.Architecture arch s)
                        => S BrickUIState arch0 s
                        -> C.AnalysisResult arch s
                        -> Seq.Seq T.Text
                        -> AppState
                        -> C.SomeUIMode
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
           , Just Refl <- testEquality (sNonce oldArchState) (C.archNonce ares) ->
             Just (oldArchState { sAnalysisResult = ares })
           | otherwise -> do
               defFunc <- listToMaybe (C.functions ares)
               b0 <- listToMaybe (C.functionBlocks ares defFunc)
               let uiState = BrickUIState { sBlockSelector = BS.emptyBlockSelector
                                          , sBlockViewer = BV.blockViewer (BlockViewerList 0) b0
                                          , sFunctionViewer = FV.functionViewer defFunc ares
                                          , sMinibuffer = MB.minibuffer MinibufferEditor MinibufferCompletionList "M-x" C.allCommands
                                          , sFunctionSelector = FS.functionSelector (const (return ())) focusedListAttr []
                                          }
               return ArchState { sAnalysisResult = ares
                                , sKeymap = C.defaultKeymap
                                , sNonce = C.archNonce ares
                                , sUIState = uiState
                                }
    }
