{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
module Surveyor ( surveyor ) where

import qualified Brick as B
import qualified Brick.BChan as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as B
import           Control.Lens ( (^?), _Just )
import qualified Data.Foldable as F
import           Data.Maybe ( fromMaybe )
import qualified Data.Parameterized.Nonce as PN
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Traversable as T
import           Data.Void ( Void )
import qualified Graphics.Vty as V

import qualified Surveyor.Architecture as A
import           Surveyor.Attributes
import           Surveyor.Events ( Events(..) )
import           Surveyor.Handlers ( appHandleEvent )
import           Surveyor.Loader ( AsyncLoader, asynchronouslyLoad )
import           Surveyor.Mode
import           Surveyor.Names ( Names(..) )
import           Surveyor.State
import qualified Surveyor.Widget.BlockSelector as BS
import qualified Surveyor.Widget.BlockViewer as BV
import qualified Surveyor.Widget.EchoArea as EA
import qualified Surveyor.Widget.FunctionSelector as FS
import qualified Surveyor.Widget.Minibuffer as MB

drawSummary :: (A.Architecture arch s) => FilePath -> A.AnalysisResult arch s -> B.Widget Names
drawSummary binFileName ares =
  B.vBox (map (drawSummaryTableEntry descColWidth) tbl)
  where
    tbl = ("Target Binary", T.pack binFileName) : A.summarizeResult ares
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
drawStatusBar :: S arch s -> B.Widget Names
drawStatusBar s =
  B.withAttr statusBarAttr (B.hBox [fileNameWidget, B.padLeft B.Max statusWidget])
  where
    fileNameWidget = B.str (fromMaybe "" (sInputFile s))
    statusWidget =
      case sAppState s of
        Loading -> B.str "Loading"
        Ready -> B.str "Ready"
        AwaitingFile -> B.str "Waiting for file"

drawAppShell :: S arch s -> B.Widget Names -> [B.Widget Names]
drawAppShell s w =
  [ B.vBox [ B.borderWithLabel (title (sUIMode s)) (B.padRight B.Max (B.padBottom B.Max w))
           , drawStatusBar s
           , bottomLine
           ]
  ]
  where
    title sm =
      case sm of
        SomeMiniBuffer (MiniBuffer m') -> title (SomeUIMode m')
        SomeUIMode m' -> B.txt (prettyMode m')
    bottomLine =
      case sUIMode s of
        SomeMiniBuffer (MiniBuffer _)
          | Just mb <- s ^? lArchState . _Just . lMinibuffer ->
            MB.renderMinibuffer True mb
        _ -> EA.renderEchoArea (sEchoArea s)

appDraw :: State s -> [B.Widget Names]
appDraw (State s) =
  case sInputFile s of
    Nothing -> drawAppShell s B.emptyWidget
    Just binFileName ->
      case sArchState s of
        Nothing -> drawAppShell s B.emptyWidget
        Just archState ->
          case sUIMode s of
            SomeMiniBuffer (MiniBuffer innerMode) ->
              drawUIMode binFileName archState s innerMode
            SomeUIMode mode ->
              drawUIMode binFileName archState s mode

drawUIMode :: (A.Architecture arch s)
           => FilePath
           -> ArchState arch s
           -> S arch s
           -> UIMode NormalK
           -> [B.Widget Names]
drawUIMode binFileName archState s uim =
  case uim of
    Diags -> drawAppShell s (drawDiagnostics (sDiagnosticLog s))
    Summary -> drawAppShell s (drawSummary binFileName binfo)
    FunctionSelector -> drawAppShell s (FS.renderFunctionSelector (sFunctionSelector archState))
    BlockSelector -> drawAppShell s (BS.renderBlockSelector (sBlockSelector archState))
    BlockViewer -> drawAppShell s (BV.renderBlockViewer binfo (sBlockViewer archState))
  where
    binfo = sAnalysisResult archState

appChooseCursor :: State s -> [B.CursorLocation Names] -> Maybe (B.CursorLocation Names)
appChooseCursor _ cursors =
  case cursors of
    [c] -> Just c
    _ -> Nothing

appAttrMap :: State s -> B.AttrMap
appAttrMap _ = B.attrMap V.defAttr [ (focusedListAttr, V.blue `B.on` V.white)
                                   , (statusBarAttr, V.black `B.on` V.white)
                                   , (B.listSelectedFocusedAttr, V.blue `B.on` V.white)
                                   ]

appStartEvent :: State s -> B.EventM Names (State s)
appStartEvent s0 = return s0

updateEchoArea :: B.BChan (Events s) -> EA.EchoArea -> IO ()
updateEchoArea customEventChan ea =
  B.writeBChan customEventChan (UpdateEchoArea ea)

surveyor :: Maybe FilePath -> IO ()
surveyor mExePath = PN.withIONonceGenerator $ \ng -> do
  customEventChan <- B.newBChan 100
  let app = B.App { B.appDraw = appDraw
                  , B.appChooseCursor = appChooseCursor
                  , B.appHandleEvent = appHandleEvent
                  , B.appStartEvent = appStartEvent
                  , B.appAttrMap = appAttrMap
                  }
  mloader <- T.traverse (asynchronouslyLoad ng customEventChan) mExePath
  s0 <- emptyState mExePath mloader ng customEventChan
  let initialState = State s0
  _finalState <- B.customMain (V.mkVty V.defaultConfig) (Just customEventChan) app initialState
  return ()


emptyState :: Maybe FilePath -> Maybe AsyncLoader -> PN.NonceGenerator IO s -> B.BChan (Events s) -> IO (S Void s)
emptyState mfp mloader ng customEventChan = do
  return S { sInputFile = mfp
           , sLoader = mloader
           , sDiagnosticLog = Seq.empty
           , sEchoArea = EA.echoArea 10 (updateEchoArea customEventChan)
           , sUIMode = SomeUIMode Summary
           , sAppState = maybe AwaitingFile (const Loading) mfp
           , sEmitEvent = B.writeBChan customEventChan
           , sEventChannel = customEventChan
           , sNonceGenerator = ng
           , sArchState = Nothing
           }
