{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Surveyor.Brick.Handlers (
  appHandleEvent
  ) where

import qualified Brick as B
import           Control.Lens ( (&), (^.), (.~), (%~), (^?), _Just )
import           Control.Monad.IO.Class ( liftIO )
import qualified Control.Once as O
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

appHandleEvent :: C.State BrickUIExtension BrickUIState s -> B.BrickEvent Names (C.Events s (C.S BrickUIExtension BrickUIState)) -> B.EventM Names (B.Next (C.State BrickUIExtension BrickUIState s))
appHandleEvent (C.State s0) evt =
  case evt of
    B.AppEvent ae -> handleCustomEvent s0 ae
    B.VtyEvent vtyEvt -> handleVtyEvent (C.State s0) vtyEvt
    B.MouseDown {} -> B.continue (C.State s0)
    B.MouseUp {} -> B.continue (C.State s0)

handleVtyEvent :: C.State BrickUIExtension BrickUIState s -> V.Event -> B.EventM Names (B.Next (C.State BrickUIExtension BrickUIState s))
handleVtyEvent s0@(C.State s) evt
  | V.EvKey k mods <- evt
  , km <- s ^. C.lKeymap
  , Just (Some cmd) <- C.lookupKeyCommand (C.sUIMode s) (C.Key k mods) km
  , Just Refl <- testEquality (C.cmdArgTypes cmd) PL.Nil = do
      -- First, we try to consult the keymap.  For now, we can only handle
      -- commands that take no arguments.  Later, once we develop a notion of
      -- "current context", we can use that to call commands that take an
      -- argument.
      let sn = (C.SomeNonce . C.sNonce) <$> C.sArchState s
      liftIO (C.cmdFunc cmd (C.sEventChannel s) sn PL.Nil)
      B.continue (C.State s)
  | otherwise =
  case C.sUIMode s of
    C.SomeMiniBuffer (C.MiniBuffer oldMode)
      | mb <- s ^. C.lUIExtension . lMinibuffer -> do
          let sn = (C.SomeNonce . C.sNonce) <$> C.sArchState s
          mbs <- MB.handleMinibufferEvent evt (C.sEventChannel s) sn mb
          case mbs of
            MB.Canceled mb' -> do
              let s' = s & C.lUIMode .~ C.SomeUIMode oldMode
                         & C.lUIExtension . lMinibuffer .~ mb'
              B.continue $! C.State s'
            MB.Completed mb' -> do
              let s' = s & C.lUIExtension . lMinibuffer .~ mb'
              B.continue $! C.State s'
      | otherwise -> B.continue s0
    C.SomeUIMode C.BlockSelector
      | Just bsel <- s ^? C.lArchState . _Just . lBlockSelector -> do
          bsel' <- BS.handleBlockSelectorEvent evt bsel
          let s' = s & C.lArchState . _Just . lBlockSelector .~ bsel'
          B.continue $! C.State s'
      | otherwise -> B.continue s0
    C.SomeUIMode C.BlockViewer
      | Just bview <- s ^? C.lArchState . _Just . lBlockViewer -> do
          bv' <- BV.handleBlockViewerEvent evt bview
          let s' = s & C.lArchState . _Just . lBlockViewer .~ bv'
          B.continue $! (C.State s')
      | otherwise -> B.continue s0
    C.SomeUIMode C.FunctionSelector
      | Just fsel <- s ^? C.lArchState . _Just . lFunctionSelector -> do
          fsel' <- FS.handleFunctionSelectorEvent evt fsel
          let s' = s & C.lArchState . _Just . lFunctionSelector .~ fsel'
          B.continue $! C.State s'
      | otherwise -> B.continue s0
    C.SomeUIMode _m -> B.continue s0

handleCustomEvent :: (C.Architecture arch s) => C.S BrickUIExtension BrickUIState arch s -> C.Events s (C.S BrickUIExtension BrickUIState) -> B.EventM Names (B.Next (C.State BrickUIExtension BrickUIState s))
handleCustomEvent s0 evt =
  case evt of
    C.AnalysisFinished (C.SomeResult bar) diags ->
      let newDiags = map (\d -> T.pack ("Analysis: " ++ show d)) diags
          notification = "Finished loading file"
          s1 = stateFromAnalysisResult s0 bar (Seq.fromList newDiags <> Seq.singleton notification) C.Ready (C.SomeUIMode C.Diags)
      in B.continue (C.State s1)
    C.AnalysisProgress (C.SomeResult bar) ->
      let s1 = stateFromAnalysisResult s0 bar Seq.empty C.Loading (C.sUIMode s0)
      in B.continue (C.State s1)
    C.AnalysisFailure exn -> do
      liftIO (C.sEmitEvent s0 (C.LogDiagnostic (T.pack ("Analysis failure: " ++ show exn))))
      B.continue $! C.State s0
    C.ErrorLoadingELFHeader off msg -> do
      let t = T.pack (printf "ELF Loading error at offset 0x%x: %s" off msg)
      let s1 = s0 & C.lDiagnosticLog %~ (Seq.|> t)
      B.continue $! C.State s1
    C.ErrorLoadingELF errs -> do
      let newDiags = map (\d -> T.pack (printf "ELF Loading error: %s" (show d))) errs
      let s1 = s0 & C.lDiagnosticLog %~ (<> Seq.fromList newDiags)
      B.continue $! C.State s1
    C.ErrorLoadingLLVM s -> do
      let t = T.pack (printf "Error loading LLVM bitcode: %s" s)
      let s1 = s0 & C.lDiagnosticLog %~ (Seq.|> t)
      B.continue $! C.State s1
    C.LoadFile filename -> do
      liftIO (F.traverse_ C.cancelLoader (C.sLoader s0))
      loader <- liftIO $ C.asynchronouslyLoad (C.sNonceGenerator s0) (C.sEventChannel s0) filename
      let s1 = s0 & C.lLoader .~ Just loader
                  & C.lInputFile .~ Just filename
      B.continue $! C.State s1
    C.LoadELF filename -> do
      liftIO (F.traverse_ C.cancelLoader (C.sLoader s0))
      loader <- liftIO $ C.asynchronouslyLoadElf (C.sNonceGenerator s0) (C.sEventChannel s0) filename
      let s1 = s0 & C.lLoader .~ Just loader
                  & C.lInputFile .~ Just filename
      B.continue $! C.State s1
    C.LoadJAR filename -> do
      liftIO (F.traverse_ C.cancelLoader (C.sLoader s0))
      loader <- liftIO $ C.asynchronouslyLoadJAR (C.sNonceGenerator s0) (C.sEventChannel s0) filename
      let s1 = s0 & C.lLoader .~ Just loader
                  & C.lInputFile .~ Just filename
      B.continue $! C.State s1
    C.LoadLLVM filename -> do
      liftIO (F.traverse_ C.cancelLoader (C.sLoader s0))
      loader <- liftIO $ C.asynchronouslyLoadLLVM (C.sNonceGenerator s0) (C.sEventChannel s0) filename
      let s1 = s0 & C.lLoader .~ Just loader
                  & C.lInputFile .~ Just filename
      B.continue $! C.State s1
    C.ShowSummary -> B.continue $! C.State (s0 & C.lUIMode .~ C.SomeUIMode C.Summary)
    C.ShowDiagnostics -> B.continue $! C.State (s0 & C.lUIMode .~ C.SomeUIMode C.Diags)
    C.DescribeCommand (Some cmd) -> do
      let msg = T.pack (printf "%s: %s" (C.cmdName cmd) (C.cmdDocstring cmd))
      liftIO (C.sEmitEvent s0 (C.EchoText msg))
      let newMode =
            case C.sUIMode s0 of
              C.SomeMiniBuffer (C.MiniBuffer oldMode) -> oldMode
              C.SomeUIMode mode -> mode
      let s1 = s0 & C.lDiagnosticLog %~ (Seq.|> msg)
                  & C.lUIMode .~ C.SomeUIMode newMode
      B.continue $! C.State s1
    C.EchoText txt -> do
      ea' <- liftIO (C.setEchoAreaText (C.sEchoArea s0) txt)
      B.continue $! C.State (s0 & C.lEchoArea .~ ea')
    C.ResetEchoArea -> B.continue $! C.State (s0 & C.lEchoArea %~ C.resetEchoArea)
    C.OpenMinibuffer ->
      case C.sUIMode s0 of
        C.SomeMiniBuffer _ -> B.continue (C.State s0)
        C.SomeUIMode mode -> B.continue $! C.State (s0 & C.lUIMode .~ C.SomeMiniBuffer (C.MiniBuffer mode))

    C.ViewBlock archNonce b
      | Just oldNonce <- s0 ^? C.lArchState . _Just . C.lNonce
      , Just Refl <- testEquality archNonce oldNonce -> do
          let s1 = s0 & C.lUIMode .~ C.SomeUIMode C.BlockViewer
                      & C.lArchState . _Just . lBlockViewer .~ BV.blockViewer (BlockViewerList 0) b
          B.continue $! C.State s1
      | otherwise -> B.continue (C.State s0)
    C.ViewFunction archNonce fh
      | Just archState <- s0 ^. C.lArchState
      , ares <- archState ^. C.lAnalysisResult
      , Just Refl <- testEquality archNonce (archState ^. C.lNonce) -> do
          let s1 = s0 & C.lUIMode .~ C.SomeUIMode C.FunctionViewer
                      & C.lArchState . _Just . lFunctionViewer .~ FV.functionViewer fh ares
          B.continue $! C.State s1
      | otherwise -> B.continue (C.State s0)
    C.FindBlockContaining archNonce addr
      | Just archState <- s0 ^. C.lArchState
      , ares <- archState ^. C.lAnalysisResult
      , Just Refl <- testEquality (archState ^. C.lNonce) archNonce -> do
          liftIO (C.sEmitEvent s0 (C.LogDiagnostic (T.pack (printf "Finding block at address %s" (C.prettyAddress addr)))))
          case C.containingBlocks ares addr of
            [b] -> do
              liftIO (C.sEmitEvent s0 (C.ViewBlock archNonce b))
              B.continue (C.State s0)
            blocks -> do
              liftIO (C.sEmitEvent s0 (C.ListBlocks archNonce blocks))
              B.continue (C.State s0)
      | otherwise -> B.continue (C.State s0)
    C.ListBlocks archNonce blocks
      | Just oldNonce <- s0 ^? C.lArchState . _Just . C.lNonce
      , Just Refl <- testEquality oldNonce archNonce -> do
          let callback b = C.sEmitEvent s0 (C.ViewBlock archNonce b)
          let s1 = s0 & C.lUIMode .~ C.SomeUIMode C.BlockSelector
                      & C.lArchState . _Just . lBlockSelector .~ BS.blockSelector callback focusedListAttr blocks
          B.continue $! C.State s1
      | otherwise -> B.continue (C.State s0)

    C.FindFunctionsContaining archNonce maddr
      | Just oldArchState <- s0 ^. C.lArchState
      , oldNonce <- oldArchState ^. C.lNonce
      , Just Refl <- testEquality oldNonce archNonce ->
        let ares = oldArchState ^. C.lAnalysisResult
        in case maddr of
            Nothing -> do
              liftIO (C.sEmitEvent s0 (C.ListFunctions archNonce (C.functions ares)))
              B.continue $! C.State s0
      | otherwise -> B.continue $! C.State s0

    C.ListFunctions archNonce funcs
      | Just oldNonce <- s0 ^? C.lArchState . _Just . C.lNonce
      , Just Refl <- testEquality oldNonce archNonce -> do
          let callback f = C.sEmitEvent s0 (C.ViewFunction archNonce f)
          let s1 = s0 & C.lUIMode .~ C.SomeUIMode C.FunctionSelector
                      & C.lArchState . _Just . lFunctionSelector .~ FS.functionSelector callback focusedListAttr funcs
          B.continue (C.State s1)
      | otherwise -> B.continue (C.State s0)

    C.LogDiagnostic t ->
      B.continue $! C.State (s0 & C.lDiagnosticLog %~ (Seq.|> t))

    -- We discard async state updates if the type of the state has changed in
    -- the interim
    C.AsyncStateUpdate archNonce nfVal upd
      | Just oldNonce <- s0 ^? C.lArchState ._Just . C.lNonce
      , Just Refl <- testEquality oldNonce archNonce ->
          B.continue (C.State (upd (O.runOnce nfVal) s0))
      | otherwise -> B.continue (C.State s0)

    C.Exit -> do
      liftIO (F.traverse_ C.cancelLoader (C.sLoader s0))
      B.halt (C.State s0)

stateFromAnalysisResult :: (C.Architecture arch s)
                        => C.S BrickUIExtension BrickUIState arch0 s
                        -> C.AnalysisResult arch s
                        -> Seq.Seq T.Text
                        -> C.AppState
                        -> C.SomeUIMode
                        -> C.S BrickUIExtension BrickUIState arch s
stateFromAnalysisResult s0 ares newDiags state uiMode =
  C.S { C.sDiagnosticLog = C.sDiagnosticLog s0 <> newDiags
      , C.sUIMode = uiMode
      , C.sAppState = state
      , C.sEmitEvent = C.sEmitEvent s0
      , C.sEventChannel = C.sEventChannel s0
      , C.sNonceGenerator = C.sNonceGenerator s0
      , C.sEchoArea = C.sEchoArea s0
      , C.sInputFile = C.sInputFile s0
      , C.sLoader = C.sLoader s0
      , C.sKeymap = C.defaultKeymap
      , C.sUIExtension = uiExt
      , C.sArchState =
        case () of
          () | Just oldArchState <- C.sArchState s0
             , Just Refl <- testEquality (C.sNonce oldArchState) (C.archNonce ares) ->
               Just (oldArchState { C.sAnalysisResult = ares })
             | otherwise -> do
                 defFunc <- listToMaybe (C.functions ares)
                 let uiState = BrickUIState { sBlockSelector = BS.emptyBlockSelector
                                            , sBlockViewer = BV.emptyBlockViewer
                                            , sFunctionViewer = FV.functionViewer defFunc ares
                                            , sFunctionSelector = FS.functionSelector (const (return ())) focusedListAttr []
                                            }
                 return C.ArchState { C.sAnalysisResult = ares
                                    , C.sNonce = C.archNonce ares
                                    , C.sUIState = uiState
                                    }
      }
  where
    addrParser s = C.SomeAddress (C.archNonce ares) <$> C.parseAddress s
    uiExt = BrickUIExtension { sMinibuffer = MB.minibuffer addrParser MinibufferEditor MinibufferCompletionList "M-x" C.allCommands
                             }
