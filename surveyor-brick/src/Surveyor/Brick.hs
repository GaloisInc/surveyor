{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
module Surveyor.Brick (
  surveyor,
  surveyorWith,
  emptyState,
  emptyArchState
  ) where

import qualified Brick as B
import qualified Brick.BChan as B
import           Brick.Markup ( (@?) )
import qualified Brick.Markup as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as B
import           Control.Lens ( (&), (^.), (^?), (.~) )
import qualified Control.Lens as L
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import           Data.Maybe ( fromMaybe, mapMaybe )
import qualified Data.Parameterized.Classes as PC
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.Nonce as PN
import           Data.Parameterized.Some ( Some(..) )
import           Data.Proxy ( Proxy(..) )
import qualified Data.Text as T
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.Text as PPT
import qualified Data.Traversable as T
import           Data.Void ( Void )
import qualified Graphics.Vty as V

import           Surveyor.Brick.Attributes
import qualified Surveyor.Brick.Command as SBC
import qualified Surveyor.Brick.EchoArea as SBEA
import qualified Surveyor.Brick.Extension as SBE
import qualified Surveyor.Brick.Handlers as BH
import qualified Surveyor.Brick.Keymap as SBK
import           Surveyor.Brick.Names ( Names(..) )
import qualified Surveyor.Brick.Widget.BlockSelector as BS
import qualified Surveyor.Brick.Widget.BlockViewer as BV
import qualified Surveyor.Brick.Widget.FunctionSelector as FS
import qualified Surveyor.Brick.Widget.FunctionViewer as FV
import qualified Surveyor.Brick.Widget.InstructionSemanticsViewer as ISV
import qualified Surveyor.Brick.Widget.Minibuffer as MB
import qualified Surveyor.Brick.Widget.SymbolicExecution as SEM
import qualified Surveyor.Core as C

drawSummary :: (C.Architecture arch s) => FilePath -> C.AnalysisResult arch s -> B.Widget Names
drawSummary binFileName ares =
  B.vBox (map (drawSummaryTableEntry descColWidth) tbl)
  where
    tbl = ("Target Binary", T.pack binFileName) : C.summarizeResult ares
    descColWidth = maximum (1 : map (T.length . fst) tbl) + 4

drawSummaryTableEntry :: Int -> (T.Text, T.Text) -> B.Widget Names
drawSummaryTableEntry keyColWidth (key, val) =
  B.padRight (B.Pad (keyColWidth - T.length key)) (B.txt key) B.<+> B.txt val

drawDiagnostics :: C.LogStore -> B.Widget Names
drawDiagnostics diags = B.viewport DiagnosticView B.Vertical body
  where
    -- FIXME: Make the history length configurable
    --
    -- We do want to limit what we render, though, because pointlessly rendering
    -- a huge history really slows down brick
    body = B.vBox [ renderMessage msg
                    -- B.markup (renderSeverity logLevel) B.<+> B.txtWrap t
                  | msg <- F.toList (C.takeLogs 100 diags)
                  ]
    renderMessage msg =
      let tm = renderTime (C.logTime msg)
          sev = renderSeverity (C.logLevel (C.logMsg msg))
          comp = renderComponent (C.logSource (C.logMsg msg))
      in case C.logText (C.logMsg msg) of
        [] -> B.markup tm B.<+> B.markup sev B.<+> B.markup comp
        [txt] -> B.markup tm B.<+> B.markup sev B.<+> B.markup comp B.<+> bDocWrap txt
        txts -> B.vBox ( B.markup tm B.<+> B.markup sev B.<+> B.markup comp
                       : map bDocWrap txts
                       )
    renderComponent comp =
      case comp of
        C.Unspecified -> ""
        C.Loader -> "Loader" @? "log-component"
        C.EventHandler nm -> PPT.renderStrict (PP.layoutCompact ("Event[" <> PP.pretty nm <> "]")) @? "log-component"
        C.CommandCallback nm -> PPT.renderStrict (PP.layoutCompact ("Command[" <> PP.pretty nm <> "]")) @? "log-component"
        C.EchoAreaUpdate -> ""

    renderTime tm = PPT.renderStrict (PP.layoutCompact (PP.pretty tm)) @? "log-time"

    renderSeverity sev =
      case sev of
        C.Debug -> "DEBUG" @? "log-debug" <> ":"
        C.Info -> "INFO" @? "log-info" <> ":"
        C.Warn -> "WARN" @? "log-warning" <> ":"
        C.Error -> "ERROR" @? "log-error" <> ":"
        -- Requested messages were explicitly initiated from a user action and
        -- shouldn't have a visible tag (and are always displayed)
        C.Requested -> ""

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

bDoc :: PP.Doc ann -> B.Widget names
bDoc = B.txt . PPT.renderStrict . PP.layoutCompact

bDocWrap :: PP.Doc ann -> B.Widget names
bDocWrap = B.txtWrap . PPT.renderStrict . PP.layoutCompact

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
        C.SomeUIMode m' -> bDoc (C.prettyMode m')
    bottomLine =
      case C.sUIMode s of
        C.SomeMiniBuffer (C.MiniBuffer _)
          | mb <- s ^. C.lUIExtension . BH.minibufferG ->
            MB.renderMinibuffer True mb
        _ -> maybe B.emptyWidget B.txt (SBEA.getEchoAreaText (SBE.sEchoArea (C.sUIExtension s)))

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
  case C.sArchState s of
    Nothing ->
      drawAppShell s (drawDiagnostics (C.sLogStore s))
    Just archState ->
      let binFileName = fromMaybe "No Input File" (C.sInputFile s)
      in case C.sUIMode s of
           C.SomeMiniBuffer (C.MiniBuffer innerMode) ->
             drawUIMode binFileName archState s innerMode
           C.SomeUIMode mode ->
             drawUIMode binFileName archState s mode

drawUIMode :: ( C.Architecture arch s
              , C.CrucibleExtension arch
              )
           => FilePath
           -> C.ArchState BH.BrickUIState arch s
           -> C.S BH.BrickUIExtension BH.BrickUIState arch s
           -> C.UIMode s C.NormalK
           -> [B.Widget Names]
drawUIMode binFileName archState s uim =
  case uim of
    C.Diags -> drawAppShell s (drawDiagnostics (C.sLogStore s))
    C.Summary -> drawAppShell s (drawSummary binFileName binfo)
    C.FunctionSelector -> drawAppShell s (FS.renderFunctionSelector (archState ^. BH.functionSelectorG))
    C.BlockSelector -> drawAppShell s (BS.renderBlockSelector (archState ^. BH.blockSelectorG))
    C.BlockViewer archNonce repr
      | Just PC.Refl <- PC.testEquality archNonce (s ^. C.lNonce)
      , Just bview <- archState ^. BH.blockViewerG repr
      , Just ctx <- archState ^? C.contextG . C.currentContext
      , Just blkState <- ctx ^. C.blockStateFor repr ->
          drawAppShell s (BV.renderBlockViewer blkState bview)
      | otherwise -> drawAppShell s (B.txt (T.pack ("Missing block viewer for IR: " ++ show repr)))
    C.FunctionViewer archNonce repr
      | Just PC.Refl <- PC.testEquality archNonce (s ^. C.lNonce)
      , Just fv <- archState ^. BH.functionViewerG repr ->
        FV.withConstraints fv $ do
          drawAppShell s (FV.renderFunctionViewer binfo (archState ^. C.contextG) fv)
      | otherwise -> drawAppShell s (B.txt (T.pack ("Missing function view for IR: " ++ show repr)))
    C.SemanticsViewer ->
      drawAppShell s (ISV.renderInstructionSemanticsViewer binfo (archState ^. C.contextG) ISV.instructionSemanticsViewer)
    C.SymbolicExecutionManager
      | Just sessionID <- archState ^? C.contextL . C.currentContext . C.symExecSessionIDL
      , Just sessions <- archState ^? C.symExStateL
      , Just (Some symExState) <- C.lookupSessionState sessions sessionID
      , Just manager <- archState ^. BH.symbolicExecutionStateG . L.at sessionID -> do
          let valNames = s ^. C.lValueNames
          drawAppShell s (SEM.renderSymbolicExecutionManager manager symExState valNames)
      | Just sessionID <- archState ^? C.contextL . C.currentContext . C.symExecSessionIDL ->
        drawAppShell s (B.txt (T.pack ("No symbolic execution state for session ID " ++ show sessionID)))
      | otherwise -> drawAppShell s (B.txt (T.pack "No symbolic execution state"))
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

surveyor :: Maybe FilePath -> IO ()
surveyor mExePath = PN.withIONonceGenerator $ \ng -> do
  customEventChan <- B.newBChan 100
  let chan = C.mkChan (B.readBChan customEventChan) (B.writeBChan customEventChan)
  mloader <- T.traverse (C.asynchronouslyLoad ng chan) mExePath
  s0 <- emptyState mExePath mloader ng chan
  surveyorWith customEventChan s0

surveyorWith :: ( C.Architecture arch s
                , C.CrucibleExtension arch
                )
             => B.BChan (C.Events s (C.S BH.BrickUIExtension BH.BrickUIState))
             -> C.S BH.BrickUIExtension BH.BrickUIState arch s
             -> IO ()
surveyorWith chan s0 = do
  let app = B.App { B.appDraw = appDraw
                  , B.appChooseCursor = appChooseCursor
                  , B.appHandleEvent = BH.appHandleEvent
                  , B.appStartEvent = appStartEvent
                  , B.appAttrMap = appAttrMap
                  }
  nextLogStore <- F.foldlM (\ls t -> (\msg -> C.appendLog msg ls)
                            <$> C.timestamp (C.msgWith { C.logText = [t] }))
                 (s0 ^. C.lLogStore) introTexts
  let initialState = C.State (s0 & C.lLogStore .~ nextLogStore)
  let mkVty = V.mkVty V.defaultConfig
  initialVty <- mkVty
  _finalState <- B.customMain initialVty mkVty (Just chan) app initialState
  return ()
  where
    introTexts = [ "Surveyor: an interactive program exploration tool"
                 , " * Press M-x to bring up the command prompt"
                 , " * Use the 'load-file' command to load a program and begin"
                 , " * C-q to quit"
                 ]


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
  let uiExt = SBC.mkExtension (C.writeChan customEventChan) n0 addrParser "M-x"
  fileLogger <- C.defaultLogFile >>= C.logToFile
  return C.S { C.sInputFile = mfp
             , C.sLoader = mloader
             , C.sLogStore = mempty
             , C.sLogActions = C.LoggingActions { C.sStateLogger = C.logToState customEventChan
                                                , C.sFileLogger = Just fileLogger
                                                }
             , C.sDiagnosticLevel = C.Debug
             , C.sUIMode = C.SomeUIMode C.Diags
             , C.sAppState = maybe C.AwaitingFile (const C.Loading) mfp
             , C.sEventChannel = customEventChan
             , C.sNonceGenerator = ng
             , C.sValueNames = C.emptyValueNameMap
             , C.sKeymap = SBK.defaultKeymap Nothing
             , C.sUIExtension = uiExt
             , C.sArchState = Nothing
             , C.sArchNonce = n0
             }

emptyArchState :: forall s arch
                . (C.Architecture arch s, C.ArchConstraints arch s)
               => Maybe FilePath
               -> PN.NonceGenerator IO s
               -> PN.Nonce s arch
               -> IO (C.AnalysisResult arch s)
               -> C.Chan (C.Events s (C.S BH.BrickUIExtension BH.BrickUIState))
               -> IO (C.S BH.BrickUIExtension BH.BrickUIState arch s)
emptyArchState mfp ng n0 mkAnalysisResult chan = do
  let addrParser _s = Nothing
  let uiExt = SBC.mkExtension (C.writeChan chan) n0 addrParser "M-x"
  fileLogger <- C.defaultLogFile >>= C.logToFile

  ares <- mkAnalysisResult

  let uiState = SBE.BrickUIState { SBE.sBlockSelector = BS.emptyBlockSelector
                                 , SBE.sBlockViewers = MapF.fromList blockViewers
                                 , SBE.sFunctionViewer = MapF.fromList (funcViewers ares)
                                 , SBE.sFunctionSelector = FS.functionSelector (const (return ())) focusedListAttr []
                                 , SBE.sSymbolicExecutionState = Map.empty
                                 }
  tc0 <- C.newTranslationCache
  let ctxStk = C.emptyContextStack
  let archState = C.ArchState { C.sAnalysisResult = ares
                              , C.sContext = ctxStk
                              , C.sSymExState = C.emptySessionState
                              , C.sIRCache = tc0
                              , C.sArchDicts = MapF.fromList dicts
                              , C.sUIState = uiState
                              }

  return C.S { C.sInputFile = mfp
             , C.sLoader = Nothing
             , C.sLogStore = mempty
             , C.sLogActions = C.LoggingActions { C.sStateLogger = C.logToState chan
                                                , C.sFileLogger = Just fileLogger
                                                }
             , C.sDiagnosticLevel = C.Debug
             , C.sUIMode = C.SomeUIMode C.Diags
             , C.sAppState = maybe C.AwaitingFile (const C.Loading) mfp
             , C.sEventChannel = chan
             , C.sNonceGenerator = ng
             , C.sValueNames = C.emptyValueNameMap
             , C.sKeymap = SBK.defaultKeymap Nothing
             , C.sUIExtension = uiExt
             , C.sArchState = Just archState
             , C.sArchNonce = n0
             }
  where
    dicts :: [MapF.Pair (C.IRRepr arch) (C.ArchDict arch s)]
    dicts = MapF.Pair C.BaseRepr C.ArchDict
          : [ MapF.Pair rep C.ArchDict
            | C.SomeIRRepr rep <- C.alternativeIRs proxy
            ]

    blockViewers :: [MapF.Pair (C.IRRepr arch) (BV.BlockViewer arch s)]
    blockViewers = MapF.Pair C.BaseRepr (BV.blockViewer InteractiveBlockViewer C.BaseRepr)
                 : [ MapF.Pair rep (BV.blockViewer InteractiveBlockViewer rep)
                   | C.SomeIRRepr rep <- C.alternativeIRs proxy
                   ]

    funcViewerCallback :: forall ir . (C.ArchConstraints ir s)
                       => C.AnalysisResult arch s -> C.IRRepr arch ir -> C.FunctionHandle arch s -> C.Block ir s -> IO ()
    funcViewerCallback ares rep fh b = do
      C.emitEvent chan (C.PushContext (C.archNonce ares) fh rep b)
      C.emitEvent chan (C.ViewBlock (C.archNonce ares) rep)

    funcViewers :: C.AnalysisResult arch s -> [MapF.Pair (C.IRRepr arch) (FV.FunctionViewer arch s)]
    funcViewers ares = MapF.Pair C.BaseRepr (FV.functionViewer (funcViewerCallback ares C.BaseRepr) FunctionCFGViewer C.BaseRepr)
                     : [ MapF.Pair rep (FV.functionViewer (funcViewerCallback ares rep) FunctionCFGViewer rep)
                       | C.SomeIRRepr rep <- C.alternativeIRs proxy
                       ]

    proxy = Proxy @(arch, s)
