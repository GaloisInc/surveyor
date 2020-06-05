{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
module Surveyor.Brick (
  surveyor,
  DebuggerConfig(..),
  debuggerFeature
  ) where

import qualified Brick as B
import qualified Brick.BChan as B
import           Brick.Markup ( (@?) )
import qualified Brick.Markup as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as B
import           Control.Lens ( (^.) )
import qualified Control.Lens as L
import qualified Data.Foldable as F
import           Data.Maybe ( fromMaybe, mapMaybe )
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Map as MapF
import qualified Data.Parameterized.Nonce as PN
import           Data.Parameterized.Some ( Some(..) )
import           Data.Proxy ( Proxy(..) )
import qualified Data.Text as T
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Text.Prettyprint.Doc.Render.Text as PPT
import qualified Data.Traversable as T
import           Data.Void ( Void )
import           Fmt ( (+|), (||+) )
import qualified Fmt as Fmt
import qualified Graphics.Vty as V

import qualified Lang.Crucible.Backend as LCB
import qualified Lang.Crucible.CFG.Extension as LCCE
import qualified Lang.Crucible.FunctionHandle as CFH
import qualified Lang.Crucible.Simulator.EvalStmt as LCS
import qualified Lang.Crucible.Simulator.ExecutionTree as LCSET
import qualified What4.Expr.Builder as WEB


import           Surveyor.Brick.Attributes
import qualified Surveyor.Brick.Command as SBC
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
        [txt] -> B.markup tm B.<+> B.markup sev B.<+> B.markup comp B.<+> B.txtWrap txt
        txts -> B.vBox ( B.markup tm B.<+> B.markup sev B.<+> B.markup comp
                       : map B.txtWrap txts
                       )
    renderComponent comp =
      case comp of
        C.Unspecified -> ""
        C.Loader -> "Loader" @? "log-component"
        C.EventHandler nm -> Fmt.fmt ("Event[" +| nm ||+ "]") @? "log-component"
        C.CommandCallback nm -> Fmt.fmt ("Command[" +| nm ||+ "]") @? "log-component"
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
    C.Diags -> drawAppShell s (drawDiagnostics (C.sLogStore s))
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
    C.SymbolicExecutionManager -> do
      drawAppShell s (SEM.renderSymbolicExecutionManager (archState ^. BH.symbolicExecutionManagerG))
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
  C.emitEvent customEventChan C.ResetEchoArea

surveyor :: Maybe FilePath -> IO ()
surveyor mExePath = PN.withIONonceGenerator $ \ng -> do
  customEventChan <- B.newBChan 100
  let chan = C.mkChan (B.readBChan customEventChan) (B.writeBChan customEventChan)
  mloader <- T.traverse (C.asynchronouslyLoad ng chan) mExePath
  s0 <- emptyState mExePath mloader ng chan
  let sconf = DebuggerConfig (Proxy @Void) (Proxy @()) (error "Void loader")
  surveyorWith sconf customEventChan s0

surveyorWith :: DebuggerConfig s ext arch
             -> B.BChan (C.Events s (C.S BH.BrickUIExtension BH.BrickUIState))
             -> C.S BH.BrickUIExtension BH.BrickUIState arch s
             -> IO ()
surveyorWith (DebuggerConfig {}) chan s0 = do
  let app = B.App { B.appDraw = appDraw
                  , B.appChooseCursor = appChooseCursor
                  , B.appHandleEvent = BH.appHandleEvent
                  , B.appStartEvent = appStartEvent
                  , B.appAttrMap = appAttrMap
                  }
  let initialState = C.State s0
  let mkVty = V.mkVty V.defaultConfig
  initialVty <- mkVty
  _finalState <- B.customMain initialVty mkVty (Just chan) app initialState
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
  let uiExt = SBC.mkExtension (C.writeChan customEventChan) n0 addrParser "M-x"
  return C.S { C.sInputFile = mfp
             , C.sLoader = mloader
             , C.sLogStore = mempty
             , C.sLogActions = C.LoggingActions { C.sStateLogger = C.logToState customEventChan
                                                , C.sFileLogger = Nothing
                                                }
             , C.sDiagnosticLevel = C.Debug
             , C.sEchoArea = C.echoArea 10 (resetEchoArea customEventChan)
             , C.sUIMode = C.SomeUIMode C.Diags
             , C.sAppState = maybe C.AwaitingFile (const C.Loading) mfp
             , C.sEventChannel = customEventChan
             , C.sNonceGenerator = ng
             , C.sKeymap = SBK.defaultKeymap
             , C.sUIExtension = uiExt
             , C.sArchState = Nothing
             , C.sArchNonce = n0
             }

stateFromContext :: forall arch s p sym ext rtp f a
                  . (C.Architecture arch s, LCB.IsSymInterface sym)
                 => PN.NonceGenerator IO s
                 -> (PN.NonceGenerator IO s -> PN.Nonce s arch -> CFH.HandleAllocator -> IO (C.AnalysisResult arch s))
                 -> C.Chan (C.Events s (C.S BH.BrickUIExtension BH.BrickUIState))
                 -> LCSET.SimState p sym ext rtp f a
                 -> IO (C.S BH.BrickUIExtension BH.BrickUIState arch s)
stateFromContext ng mkAnalysisResult chan simState = do
  let simCtx = simState ^. LCSET.stateContext
  let halloc = simCtx ^. L.to LCSET.simHandleAllocator
  let addrParser _s = Nothing
  n0 <- PN.freshNonce ng
  ares <- mkAnalysisResult ng n0 halloc
  let uiExt = SBC.mkExtension (C.writeChan chan) n0 addrParser "M-x"
  let dicts = MapF.Pair C.BaseRepr C.ArchDict
            : [ MapF.Pair rep C.ArchDict
              | C.SomeIRRepr rep <- C.alternativeIRs (Proxy @(arch, s))
              ]
  tc0 <- C.newTranslationCache
  ses <- C.defaultSymbolicExecutionConfig ng
  let blockViewers = (MapF.Pair C.BaseRepr (BV.blockViewer InteractiveBlockViewer C.BaseRepr))
                     : [ MapF.Pair rep (BV.blockViewer InteractiveBlockViewer rep)
                       | C.SomeIRRepr rep <- C.alternativeIRs (Proxy @(arch, s))
                       ]
  let funcViewerCallback :: forall ir . (C.ArchConstraints ir s) => C.IRRepr arch ir -> C.FunctionHandle arch s -> C.Block ir s -> IO ()
      funcViewerCallback rep fh b = do
        C.emitEvent chan (C.PushContext (C.archNonce ares) fh rep b)
        C.emitEvent chan (C.ViewBlock (C.archNonce ares) rep)
  let funcViewers = (MapF.Pair C.BaseRepr (FV.functionViewer (funcViewerCallback C.BaseRepr) FunctionCFGViewer C.BaseRepr))
                    : [ MapF.Pair rep (FV.functionViewer (funcViewerCallback rep) FunctionCFGViewer rep)
                      | C.SomeIRRepr rep <- C.alternativeIRs (Proxy @(arch, s))
                      ]
  let uiState = SBE.BrickUIState { SBE.sBlockSelector = BS.emptyBlockSelector
                                 , SBE.sBlockViewers = MapF.fromList blockViewers
                                 , SBE.sFunctionViewer = MapF.fromList funcViewers
                                 , SBE.sFunctionSelector = FS.functionSelector (const (return ())) focusedListAttr []
                                 , SBE.sSymbolicExecutionManager =
                                   SEM.symbolicExecutionManager (Some (C.Configuring ses))
                                 }
  sesID <- C.newSessionID ng
  -- FIXME: Change this to a better ADT that reflects that we don't have this
  -- config (we inherit it from the existing session)
  let symCfg = C.SymbolicExecutionConfig sesID undefined WEB.FloatRealRepr undefined
  let symSt = C.SymbolicState { C.symbolicConfig = symCfg
                              , C.symbolicBackend = simCtx ^. LCSET.ctxSymInterface
                              , C.withSymConstraints = \a -> a
                              -- FIXME: We should be able to pull these out of the 'ActiveTree' in simState
                              , C.someCFG = undefined
                              , C.symbolicRegs = undefined
                              , C.symbolicGlobals = undefined
                              }
  let archState = C.ArchState { C.sAnalysisResult = ares
                              -- FIXME: Pick a context based on the stack in the simulator
                              , C.sContext = C.emptyContextStack
                              -- FIXME: Construct a session for this with whatever we can pull out of simState
                              , C.sSymExState = C.singleSessionState (C.Suspended symSt)
                              , C.sIRCache = tc0
                              , C.sArchDicts = MapF.fromList dicts
                              , C.sUIState = uiState
                              }
  return C.S { C.sInputFile = Nothing
             , C.sLoader = Nothing
             , C.sLogStore = mempty
             , C.sLogActions = C.LoggingActions { C.sStateLogger = C.logToState chan
                                                , C.sFileLogger = Nothing
                                                }
             , C.sDiagnosticLevel = C.Debug
             , C.sEchoArea = C.echoArea 10 (resetEchoArea chan)
             , C.sAppState = C.Ready
             , C.sNonceGenerator = ng
             , C.sKeymap = SBK.defaultKeymap
             , C.sArchNonce = n0
             , C.sEventChannel = chan
             , C.sUIExtension = uiExt
             , C.sArchState = Just archState
              -- FIXME: choose the most appropriate view given the crucible state?
               --
               -- If we have a SimState, show the symbolic execution suspended state viewer
               --
               -- Otherwise, show the term inspector
             , C.sUIMode = C.SomeUIMode C.Diags
             }



-- | This crucible 'LCS.ExecutionFeature' captures the simulator state at a
-- breakpoint and starts up Surveyor(-Brick) to interactively inspect it.
--
-- For now, it simply returns execution to Crucible with no modifications after
-- Surveyor exits.  It could be extended in the future to persist state
-- modifications.
debuggerFeature :: forall ext sym p rtp arch
                 . (LCB.IsSymInterface sym, LCCE.IsSyntaxExtension ext)
                => DebuggerConfig PN.GlobalNonceGenerator ext arch
                -> LCS.ExecutionFeature p sym ext rtp
debuggerFeature args = LCS.ExecutionFeature (debugger args)

data DebuggerConfig s ext arch =
  (C.Architecture arch s, ext ~ C.CrucibleExt arch) =>
  DebuggerConfig { _debuggerArchProxy :: Proxy arch
                 , _debuggerExtProxy :: Proxy ext
                 , debuggerCon :: PN.NonceGenerator IO s -> PN.Nonce s arch -> CFH.HandleAllocator -> IO (C.AnalysisResult arch s)
                 }

data BreakpointType = UnconditionalBreakpoint | ConditionalBreakpoint

-- | Classify a call as a known breakpoint (or not)
--
-- All of our breakpoints are overrides with distinguished names
classifyBreakpoint :: LCSET.ResolvedCall p sym ext ret -> Maybe BreakpointType
classifyBreakpoint rc =
  case rc of
    LCSET.CrucibleCall {} -> Nothing
    LCSET.OverrideCall o _fr
      | LCSET.overrideName o == "crux_breakpoint" -> Just UnconditionalBreakpoint
      | LCSET.overrideName o == "crux_breakpoint_if" -> Just ConditionalBreakpoint
      | otherwise -> Nothing

debugger :: (LCB.IsSymInterface sym, LCCE.IsSyntaxExtension ext)
         => DebuggerConfig PN.GlobalNonceGenerator ext arch
         -> LCSET.ExecState p sym ext rtp
         -> IO (LCS.ExecutionFeatureResult p sym ext rtp)
debugger args@(DebuggerConfig {}) execSt =
  case execSt of
    LCSET.CallState _retHdlr resolvedCall simState
      | Just UnconditionalBreakpoint <- classifyBreakpoint resolvedCall ->
        surveyorState args simState
    LCSET.TailCallState _v resolvedCall simState
      | Just UnconditionalBreakpoint <- classifyBreakpoint resolvedCall->
        surveyorState args simState
    _ -> return LCS.ExecutionFeatureNoChange

-- | Initialize Surveyor to navigate an active symbolic execution state
surveyorState :: (C.Architecture arch PN.GlobalNonceGenerator, LCB.IsSymInterface sym)
              => DebuggerConfig PN.GlobalNonceGenerator ext arch
              -> LCSET.SimState p sym ext rtp f a
              -> IO (LCS.ExecutionFeatureResult p sym ext rtp)
surveyorState args simCtx = do
  customEventChan <- B.newBChan 100
  let chan = C.mkChan (B.readBChan customEventChan) (B.writeBChan customEventChan)
  s0 <- stateFromContext PN.globalNonceGenerator (debuggerCon args) chan simCtx
  surveyorWith args customEventChan s0
  return LCS.ExecutionFeatureNoChange
