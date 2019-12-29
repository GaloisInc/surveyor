{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
module Surveyor.Brick ( surveyor ) where

import qualified Brick as B
import qualified Brick.Markup as B
import           Brick.Markup ( (@?) )
import qualified Brick.BChan as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as B
import           Control.Lens ( (^.) )
import qualified Data.Foldable as F
import           Data.Maybe ( fromMaybe, mapMaybe )
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Nonce as PN
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Traversable as T
import           Data.Void ( Void )
import qualified Graphics.Vty as V

import qualified Surveyor.Core as C
import           Surveyor.Brick.Attributes
import qualified Surveyor.Brick.Handlers as BH
import           Surveyor.Brick.Names ( Names(..) )
import qualified Surveyor.Brick.Widget.BlockSelector as BS
import qualified Surveyor.Brick.Widget.BlockViewer as BV
import qualified Surveyor.Brick.Widget.FunctionSelector as FS
import qualified Surveyor.Brick.Widget.FunctionViewer as FV
import qualified Surveyor.Brick.Widget.InstructionSemanticsViewer as ISV
import qualified Surveyor.Brick.Widget.Minibuffer as MB

drawSummary :: (C.Architecture arch s) => FilePath -> C.AnalysisResult arch s -> B.Widget Names
drawSummary binFileName ares =
  B.vBox (map (drawSummaryTableEntry descColWidth) tbl)
  where
    tbl = ("Target Binary", T.pack binFileName) : C.summarizeResult ares
    descColWidth = maximum (1 : map (T.length . fst) tbl) + 4

drawSummaryTableEntry :: Int -> (T.Text, T.Text) -> B.Widget Names
drawSummaryTableEntry keyColWidth (key, val) =
  B.padRight (B.Pad (keyColWidth - T.length key)) (B.txt key) B.<+> B.txt val

drawDiagnostics :: Seq.Seq (Maybe C.LogLevel, T.Text) -> B.Widget Names
drawDiagnostics diags = B.viewport DiagnosticView B.Vertical body
  where
    body = B.vBox [ B.markup (renderLogLevel logLevel) B.<+> B.txtWrap t
                  | (logLevel, t) <- F.toList diags
                  ]
    renderLogLevel mll =
      case mll of
        Nothing -> ""
        Just ll ->
          case ll of
            C.LogDebug -> "DEBUG" @? "log-debug" <> ":"
            C.LogInfo -> "INFO" @? "log-info" <> ":"
            C.LogWarning -> "WARN" @? "log-warning" <> ":"
            C.LogError -> "ERROR" @? "log-error" <> ":"
            C.LogFatal -> "FATAL" @? "log-fatal" <> ":"

-- | Draw a status bar based on the current state
--
-- The status bar is a line at the bottom of the screen that reflects the
-- currently-loaded executable (if any) and includes an indicator of the
-- analysis progress.
drawStatusBar :: C.S BH.BrickUIExtension BH.BrickUIState arch s -> B.Widget Names
drawStatusBar s =
  B.withAttr statusBarAttr (B.hBox [fileNameWidget, B.padLeft B.Max statusWidget])
  where
    fileNameWidget = B.str (fromMaybe "" (C.sInputFile s))
    statusWidget =
      case C.sAppState s of
        C.Loading -> B.str "Loading"
        C.Ready -> B.str "Ready"
        C.AwaitingFile -> B.str "Waiting for file"

drawAppShell :: (C.Architecture arch s) => C.S BH.BrickUIExtension BH.BrickUIState arch s -> B.Widget Names -> [B.Widget Names]
drawAppShell s w =
  [ B.vBox [ B.borderWithLabel (title (C.sUIMode s)) (B.padRight B.Max (B.padBottom B.Max w))
           , drawKeyBindings s
           , drawStatusBar s
           , bottomLine
           ]
  ]
  where
    title sm =
      case sm of
        C.SomeMiniBuffer (C.MiniBuffer m') -> title (C.SomeUIMode m')
        C.SomeUIMode m' -> B.txt (C.prettyMode m')
    bottomLine =
      case C.sUIMode s of
        C.SomeMiniBuffer (C.MiniBuffer _)
          | mb <- s ^. C.lUIExtension . BH.minibufferG ->
            MB.renderMinibuffer True mb
        _ -> maybe B.emptyWidget B.txt (C.getEchoAreaText (C.sEchoArea s))

drawKeyBindings :: (C.Architecture arch s) => C.S BH.BrickUIExtension BH.BrickUIState arch s -> B.Widget Names
drawKeyBindings s = B.hBox (mapMaybe toKeyHint keys)
  where
    keys = C.modeKeybindings (s ^. C.lKeymap) (s ^. C.lUIMode)
    toKeyHint (k, C.SomeCommand cmd)
      | isPlainKey k && C.cmdApplicable cmd (C.SomeState s) =
        Just (B.hBox [B.str (show (PP.pretty k) ++ ": "), B.txt (C.cmdName cmd), B.str "  "])
      | otherwise = Nothing

isPlainKey :: C.Key -> Bool
isPlainKey (C.Key k ms) =
  case k of
    V.KChar _ -> null ms
    _ -> False

appDraw :: C.State BH.BrickUIExtension BH.BrickUIState s -> [B.Widget Names]
appDraw (C.State s) =
  case C.sInputFile s of
    Nothing -> drawAppShell s introText
    Just binFileName ->
      case C.sArchState s of
        Nothing -> drawAppShell s introText
        Just archState ->
          case C.sUIMode s of
            C.SomeMiniBuffer (C.MiniBuffer innerMode) ->
              drawUIMode binFileName archState s innerMode
            C.SomeUIMode mode ->
              drawUIMode binFileName archState s mode
  where
    introText = B.str $ unlines ["Surveyor: an interactive program exploration tool"
                                , " * Press M-x to bring up the command prompt"
                                , " * Use the 'load-file' command to load a program and begin"
                                , " * C-q to quit"
                                ]

drawUIMode :: (C.Architecture arch s)
           => FilePath
           -> C.ArchState BH.BrickUIState arch s
           -> C.S BH.BrickUIExtension BH.BrickUIState arch s
           -> C.UIMode s C.NormalK
           -> [B.Widget Names]
drawUIMode binFileName archState s uim =
  case uim of
    C.Diags -> drawAppShell s (drawDiagnostics (C.sDiagnosticLog s))
    C.Summary -> drawAppShell s (drawSummary binFileName binfo)
    C.FunctionSelector -> drawAppShell s (FS.renderFunctionSelector (archState ^. BH.functionSelectorG))
    C.BlockSelector -> drawAppShell s (BS.renderBlockSelector (archState ^. BH.blockSelectorG))
    C.BlockViewer archNonce repr
      | Just Refl <- testEquality archNonce (s ^. C.lNonce)
      , Just bview <- archState ^. BH.blockViewerG repr ->
          drawAppShell s (BV.renderBlockViewer binfo (archState ^. C.contextG) bview)
      | otherwise -> drawAppShell s (B.txt (T.pack ("Missing block viewer for IR: " ++ show repr)))
    C.FunctionViewer archNonce repr
      | Just Refl <- testEquality archNonce (s ^. C.lNonce)
      , Just fv <- archState ^. BH.functionViewerG repr ->
        FV.withConstraints fv $ do
          drawAppShell s (FV.renderFunctionViewer binfo (archState ^. C.contextG) fv)
      | otherwise -> drawAppShell s (B.txt (T.pack ("Missing function view for IR: " ++ show repr)))
    C.SemanticsViewer ->
      drawAppShell s (ISV.renderInstructionSemanticsViewer binfo (archState ^. C.contextG) ISV.instructionSemanticsViewer)
  where
    binfo = C.sAnalysisResult archState

appChooseCursor :: C.State BH.BrickUIExtension BH.BrickUIState s -> [B.CursorLocation Names] -> Maybe (B.CursorLocation Names)
appChooseCursor _ cursors =
  case cursors of
    [c] -> Just c
    _ -> Nothing

appAttrMap :: C.State BH.BrickUIExtension BH.BrickUIState s -> B.AttrMap
appAttrMap _ = B.attrMap V.defAttr [ (focusedListAttr, V.blue `B.on` V.white)
                                   , (statusBarAttr, V.black `B.on` V.white)
                                   , (B.listSelectedFocusedAttr, V.blue `B.on` V.white)
                                   , ("log-debug", B.fg V.magenta)
                                   , ("log-info", B.fg V.blue)
                                   , ("log-warn", B.fg V.yellow)
                                   , ("log-error", B.fg V.red)
                                   , ("log-fatal", V.withStyle (B.fg V.red) V.bold)
                                   ]

appStartEvent :: C.State BH.BrickUIExtension BH.BrickUIState s -> B.EventM Names (C.State BH.BrickUIExtension BH.BrickUIState s)
appStartEvent s0 = return s0

resetEchoArea :: C.Chan (C.Events s (C.S BH.BrickUIExtension BH.BrickUIState)) -> IO ()
resetEchoArea customEventChan =
  C.writeChan customEventChan C.ResetEchoArea

surveyor :: Maybe FilePath -> IO ()
surveyor mExePath = PN.withIONonceGenerator $ \ng -> do
  customEventChan <- B.newBChan 100
  let chan = C.mkChan (B.readBChan customEventChan) (B.writeBChan customEventChan)
  let app = B.App { B.appDraw = appDraw
                  , B.appChooseCursor = appChooseCursor
                  , B.appHandleEvent = BH.appHandleEvent
                  , B.appStartEvent = appStartEvent
                  , B.appAttrMap = appAttrMap
                  }
  mloader <- T.traverse (C.asynchronouslyLoad ng chan) mExePath
  s0 <- emptyState mExePath mloader ng chan
  let initialState = C.State s0
  let mkVty = V.mkVty V.defaultConfig
  initialVty <- mkVty
  _finalState <- B.customMain initialVty mkVty (Just customEventChan) app initialState
  return ()


emptyState :: Maybe FilePath
           -> Maybe C.AsyncLoader
           -> PN.NonceGenerator IO s
           -> C.Chan (C.Events s (C.S BH.BrickUIExtension BH.BrickUIState))
           -> IO (C.S BH.BrickUIExtension BH.BrickUIState Void s)
emptyState mfp mloader ng customEventChan = do
  let addrParser _s = Nothing
  -- We need a nonce for the minibuffer, but we don't have one here yet.  Just
  -- make one (that won't match anything else)
  n0 <- PN.freshNonce ng
  let uiExt = BH.mkExtension (C.writeChan customEventChan) n0 addrParser "M-x"
  return C.S { C.sInputFile = mfp
             , C.sLoader = mloader
             , C.sDiagnosticLog = Seq.empty
             , C.sDiagnosticLevel = C.LogDebug
             , C.sEchoArea = C.echoArea 10 (resetEchoArea customEventChan)
             , C.sUIMode = C.SomeUIMode C.Diags
             , C.sAppState = maybe C.AwaitingFile (const C.Loading) mfp
             , C.sEmitEvent = C.writeChan customEventChan
             , C.sEventChannel = customEventChan
             , C.sNonceGenerator = ng
             , C.sKeymap = C.defaultKeymap
             , C.sUIExtension = uiExt
             , C.sArchState = Nothing
             , C.sArchNonce = n0
             }
