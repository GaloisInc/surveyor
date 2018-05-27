{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
module Surveyor.Brick ( surveyor ) where

import qualified Brick as B
import qualified Brick.BChan as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as B
import           Control.Lens ( (^.), (^?), _Just )
import qualified Data.Foldable as F
import           Data.Maybe ( fromMaybe )
import qualified Data.Parameterized.Nonce as PN
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Traversable as T
import           Data.Void ( Void )
import qualified Graphics.Vty as V

import qualified Surveyor.Core as C
import           Surveyor.Brick.Attributes
import           Surveyor.Brick.Handlers ( appHandleEvent )
import           Surveyor.Brick.Names ( Names(..) )
import           Surveyor.Brick.State
import qualified Surveyor.Brick.Widget.BlockSelector as BS
import qualified Surveyor.Brick.Widget.BlockViewer as BV
import qualified Surveyor.Brick.Widget.FunctionSelector as FS
import qualified Surveyor.Brick.Widget.FunctionViewer as FV
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

drawDiagnostics :: Seq.Seq T.Text -> B.Widget Names
drawDiagnostics diags = B.viewport DiagnosticView B.Vertical body
  where
    body = B.vBox [ B.txtWrap t | t <- F.toList diags ]

-- | Draw a status bar based on the current state
--
-- The status bar is a line at the bottom of the screen that reflects the
-- currently-loaded executable (if any) and includes an indicator of the
-- analysis progress.
drawStatusBar :: S BrickUIState arch s -> B.Widget Names
drawStatusBar s =
  B.withAttr statusBarAttr (B.hBox [fileNameWidget, B.padLeft B.Max statusWidget])
  where
    fileNameWidget = B.str (fromMaybe "" (sInputFile s))
    statusWidget =
      case sAppState s of
        Loading -> B.str "Loading"
        Ready -> B.str "Ready"
        AwaitingFile -> B.str "Waiting for file"

drawAppShell :: S BrickUIState arch s -> B.Widget Names -> [B.Widget Names]
drawAppShell s w =
  [ B.vBox [ B.borderWithLabel (title (sUIMode s)) (B.padRight B.Max (B.padBottom B.Max w))
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
      case sUIMode s of
        C.SomeMiniBuffer (C.MiniBuffer _)
          | Just mb <- s ^? lArchState . _Just . lMinibuffer ->
            MB.renderMinibuffer True mb
        _ -> maybe B.emptyWidget B.txt (C.getEchoAreaText (sEchoArea s))

appDraw :: State BrickUIState s -> [B.Widget Names]
appDraw (State s) =
  case sInputFile s of
    Nothing -> drawAppShell s B.emptyWidget
    Just binFileName ->
      case sArchState s of
        Nothing -> drawAppShell s B.emptyWidget
        Just archState ->
          case sUIMode s of
            C.SomeMiniBuffer (C.MiniBuffer innerMode) ->
              drawUIMode binFileName archState s innerMode
            C.SomeUIMode mode ->
              drawUIMode binFileName archState s mode

drawUIMode :: (C.Architecture arch s)
           => FilePath
           -> ArchState BrickUIState arch s
           -> S BrickUIState arch s
           -> C.UIMode C.NormalK
           -> [B.Widget Names]
drawUIMode binFileName archState s uim =
  case uim of
    C.Diags -> drawAppShell s (drawDiagnostics (sDiagnosticLog s))
    C.Summary -> drawAppShell s (drawSummary binFileName binfo)
    C.FunctionSelector -> drawAppShell s (FS.renderFunctionSelector (archState ^. lFunctionSelector))
    C.BlockSelector -> drawAppShell s (BS.renderBlockSelector (archState ^. lBlockSelector))
    C.BlockViewer -> drawAppShell s (BV.renderBlockViewer binfo (archState ^. lBlockViewer))
    C.FunctionViewer -> drawAppShell s (FV.renderFunctionViewer (archState ^. lFunctionViewer ))
  where
    binfo = sAnalysisResult archState

appChooseCursor :: State BrickUIState s -> [B.CursorLocation Names] -> Maybe (B.CursorLocation Names)
appChooseCursor _ cursors =
  case cursors of
    [c] -> Just c
    _ -> Nothing

appAttrMap :: State BrickUIState s -> B.AttrMap
appAttrMap _ = B.attrMap V.defAttr [ (focusedListAttr, V.blue `B.on` V.white)
                                   , (statusBarAttr, V.black `B.on` V.white)
                                   , (B.listSelectedFocusedAttr, V.blue `B.on` V.white)
                                   ]

appStartEvent :: State BrickUIState s -> B.EventM Names (State BrickUIState s)
appStartEvent s0 = return s0

resetEchoArea :: C.Chan (C.Events s) -> IO ()
resetEchoArea customEventChan =
  C.writeChan customEventChan C.ResetEchoArea

surveyor :: Maybe FilePath -> IO ()
surveyor mExePath = PN.withIONonceGenerator $ \ng -> do
  customEventChan <- B.newBChan 100
  let chan = C.mkChan (B.readBChan customEventChan) (B.writeBChan customEventChan)
  let app = B.App { B.appDraw = appDraw
                  , B.appChooseCursor = appChooseCursor
                  , B.appHandleEvent = appHandleEvent
                  , B.appStartEvent = appStartEvent
                  , B.appAttrMap = appAttrMap
                  }
  mloader <- T.traverse (C.asynchronouslyLoad ng chan) mExePath
  s0 <- emptyState mExePath mloader ng chan
  let initialState = State s0
  _finalState <- B.customMain (V.mkVty V.defaultConfig) (Just customEventChan) app initialState
  return ()


emptyState :: Maybe FilePath -> Maybe C.AsyncLoader -> PN.NonceGenerator IO s -> C.Chan (C.Events s) -> IO (S BrickUIState Void s)
emptyState mfp mloader ng customEventChan = do
  return S { sInputFile = mfp
           , sLoader = mloader
           , sDiagnosticLog = Seq.empty
           , sEchoArea = C.echoArea 10 (resetEchoArea customEventChan)
           , sUIMode = C.SomeUIMode C.Summary
           , sAppState = maybe AwaitingFile (const Loading) mfp
           , sEmitEvent = C.writeChan customEventChan
           , sEventChannel = customEventChan
           , sNonceGenerator = ng
           , sArchState = Nothing
           }
