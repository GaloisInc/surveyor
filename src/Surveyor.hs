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
import qualified Brick.Widgets.List as B
import qualified Control.Lens as L
import           Control.Monad.IO.Class ( liftIO )
import qualified Data.Foldable as F
import qualified Data.Map as M
import           Data.Maybe ( fromMaybe )
import           Data.Monoid
import qualified Data.Parameterized.List as PL
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Traversable as T
import qualified Data.Vector as V
import qualified Graphics.Vty as V
import           Text.Printf ( printf )

import qualified Data.Macaw.Discovery as MD
import qualified Data.Macaw.Memory as MM
import qualified Renovate as R

import           Surveyor.Attributes
import           Surveyor.BinaryAnalysisResult ( BinaryAnalysisResult(..)
                                               , BinaryAnalysisResultWrapper(..)
                                               )
import           Surveyor.Events ( Events(..) )
import           Surveyor.Loader ( asynchronouslyLoad )
import qualified Surveyor.Minibuffer as MB
import           Surveyor.Mode
import           Surveyor.State

drawSummary :: FilePath -> BinaryAnalysisResult i a w arch -> B.Widget Names
drawSummary binFileName BinaryAnalysisResult { rBlockInfo = binfo } =
  B.vBox [ B.str ("Target binary: " ++ binFileName)
         , B.str ("Discovered functions: " ++ show (length (R.biFunctionEntries binfo)))
         , B.str ("Discovered blocks: " ++ show (length (R.biBlocks binfo)))
         ]

drawConcreteBlock :: (MM.MemWidth w) => R.ISA i a w -> R.ConcreteBlock i w -> B.Widget Names
drawConcreteBlock isa b =
  B.vBox [ B.str (printf "Block address: %s" (show (R.basicBlockAddress b)))
         , B.vBox [ B.str (R.isaPrettyInstruction isa i) | i <- R.basicBlockInstructions b ]
         ]

drawFunctionList :: (MM.MemWidth w) => S i a w arch -> BinaryAnalysisResult i a w arch -> B.Widget Names
drawFunctionList S { sFunctionList = flist }
                 BinaryAnalysisResult { rBlockInfo = binfo, rISA = isa } =
  B.renderList drawFunctionEntry True flist
  where
    drawFunctionEntry isFocused (FLE addr txt blockCount) =
      let focusedXfrm = if isFocused then B.withAttr focusedListAttr else id
      in focusedXfrm (B.hBox [B.str (printf "%s: %s (%d blocks)" (show (PP.pretty addr)) (T.unpack txt) blockCount)])

drawDiagnostics :: Seq.Seq T.Text -> B.Widget Names
drawDiagnostics diags = B.viewport DiagnosticView B.Vertical body
  where
    body = B.vBox [ B.txtWrap t | t <- F.toList diags ]

-- | Draw a status bar based on the current state
--
-- The status bar is a line at the bottom of the screen that reflects the
-- currently-loaded executable (if any) and includes an indicator of the
-- analysis progress.
drawStatusBar :: S i a w arch -> B.Widget Names
drawStatusBar s =
  B.withAttr statusBarAttr (B.hBox [fileNameWidget, B.padLeft B.Max statusWidget])
  where
    fileNameWidget = B.str (fromMaybe "" (sInputFile s))
    statusWidget =
      case sAppState s of
        Loading -> B.str "Loading"
        Ready -> B.str "Ready"
        AwaitingFile -> B.str "Waiting for file"

drawEchoArea :: S i a w arch -> B.Widget Names
drawEchoArea s =
  case Seq.viewr (sDiagnosticLog s) of
    Seq.EmptyR -> B.emptyWidget
    _ Seq.:> lastDiag -> B.txt lastDiag

drawAppShell :: S i a w arch -> B.Widget Names -> [B.Widget Names]
drawAppShell s w = [B.vBox [ B.padBottom B.Max w
                           , drawStatusBar s
                           , bottomLine
                           ]
                   ]
  where
    bottomLine =
      case sUIMode s of
        SomeMiniBuffer (MiniBuffer _) -> MB.renderMinibuffer True (sMinibuffer s)
        _ ->  drawEchoArea s

appDraw :: State -> [B.Widget Names]
appDraw (State s) =
  case sInputFile s of
    Nothing -> drawAppShell s B.emptyWidget
    Just binFileName ->
      case sBinaryInfo s of
        Nothing -> drawAppShell s B.emptyWidget
        Just binfo ->
          case sUIMode s of
            SomeMiniBuffer (MiniBuffer innerMode) ->
              drawUIMode binFileName binfo s innerMode
            SomeUIMode mode ->
              drawUIMode binFileName binfo s mode

drawUIMode :: (MM.MemWidth w)
           => FilePath
           -> BinaryAnalysisResult i a w arch
           -> S i a w arch
           -> UIMode NormalK
           -> [B.Widget Names]
drawUIMode binFileName binfo s uim =
  case uim of
    Diags -> drawAppShell s (drawDiagnostics (sDiagnosticLog s))
    Summary -> drawAppShell s (drawSummary binFileName binfo)
    ListFunctions -> drawAppShell s (drawFunctionList s binfo)

appChooseCursor :: State -> [B.CursorLocation Names] -> Maybe (B.CursorLocation Names)
appChooseCursor _ _ = Nothing

appAttrMap :: State -> B.AttrMap
appAttrMap _ = B.attrMap V.defAttr [ (focusedListAttr, B.bg V.blue <> B.fg V.white)
                                   , (statusBarAttr, B.bg V.black <> B.fg V.white)
                                   ]

appHandleEvent :: State -> B.BrickEvent Names Events -> B.EventM Names (B.Next State)
appHandleEvent (State s0) evt =
  case evt of
    B.AppEvent ae ->
      case ae of
        AnalysisFinished (BinaryAnalysisResultWrapper bar@BinaryAnalysisResult { rBlockInfo = rbi }) diags ->
          let newDiags = map (\d -> T.pack ("Analysis: " ++ show d)) diags
              notification = "Finished loading file"
              funcList = V.fromList [ FLE addr textName blockCount
                                    | (addr, Some dfi) <- M.toList (R.biDiscoveryFunInfo rbi)
                                    , let textName = TE.decodeUtf8With TE.lenientDecode (MD.discoveredFunName dfi)
                                    , let blockCount = M.size (dfi L.^. MD.parsedBlocks)
                                    ]
          in B.continue $ State S { sBinaryInfo = Just bar
                                  , sFunctionList = B.list FunctionList funcList 1
                                  , sDiagnosticLog =
                                    sDiagnosticLog s0 <> Seq.fromList newDiags <> Seq.singleton notification
                                  , sUIMode = SomeUIMode Diags
                                  , sInputFile = sInputFile s0
                                  , sMinibuffer = sMinibuffer s0
                                  , sAppState = Ready
                                  , sEmitEvent = sEmitEvent s0
                                  }
        AnalysisProgress _addr (BinaryAnalysisResultWrapper bar@BinaryAnalysisResult { rBlockInfo = rbi }) ->
          let funcList = V.fromList [ FLE addr textName blockCount
                                    | (addr, Some dfi) <- M.toList (R.biDiscoveryFunInfo rbi)
                                    , let textName = TE.decodeUtf8With TE.lenientDecode (MD.discoveredFunName dfi)
                                    , let blockCount = M.size (dfi L.^. MD.parsedBlocks)
                                    ]
          in B.continue $ State S { sBinaryInfo = Just bar
                                  , sFunctionList = B.list FunctionList funcList 1
                                  , sMinibuffer = sMinibuffer s0
                                  , sDiagnosticLog = sDiagnosticLog s0
                                  , sUIMode = sUIMode s0
                                  , sInputFile = sInputFile s0
                                  , sAppState = Loading
                                  , sEmitEvent = sEmitEvent s0
                                  }
        BlockDiscovered addr ->
          B.continue $ State s0 { sDiagnosticLog = sDiagnosticLog s0 Seq.|> T.pack ("Found a block at address " ++ show addr) }
        AnalysisFailure exn ->
          B.continue $ State s0 { sDiagnosticLog = sDiagnosticLog s0 Seq.|> T.pack ("Analysis failure: " ++ show exn) }
        ErrorLoadingELFHeader off msg ->
          let t = T.pack (printf "ELF Loading error at offset 0x%x: %s" off msg)
          in B.continue $ State s0 { sDiagnosticLog = sDiagnosticLog s0 Seq.|> t }
        ErrorLoadingELF errs ->
          let newDiags = map (\d -> T.pack (printf "ELF Loading error: %s" (show d))) errs
          in B.continue $ State s0 { sDiagnosticLog = sDiagnosticLog s0 <> Seq.fromList newDiags }
        ShowSummary -> B.continue $ State s0 { sUIMode = SomeUIMode Summary }
        ShowDiagnostics -> B.continue $ State s0 { sUIMode = SomeUIMode Diags }
        Exit -> B.halt (State s0)
    B.VtyEvent vtyEvt -> handleVtyEvent (State s0) vtyEvt
    B.MouseDown {} -> B.continue (State s0)
    B.MouseUp {} -> B.continue (State s0)

isListEventKey :: V.Key -> Bool
isListEventKey k =
  case k of
    V.KUp -> True
    V.KDown -> True
    V.KHome -> True
    V.KEnd -> True
    V.KPageDown -> True
    V.KPageUp -> True
    _ -> False

handleVtyEvent :: State -> V.Event -> B.EventM Names (B.Next State)
handleVtyEvent s0@(State (s@S { sFunctionList = l0 })) evt =
  case sUIMode s of
    SomeMiniBuffer (MiniBuffer oldMode) ->
      case evt of
        V.EvKey (V.KChar 'q') [V.MCtrl] -> do
          liftIO (sEmitEvent s Exit)
          B.continue (State s)
        _ -> do
          mbs <- MB.handleMinibufferEvent evt (sMinibuffer s)
          case mbs of
            MB.Canceled mb' ->
              B.continue $ State s { sMinibuffer = mb'
                                   , sUIMode = SomeUIMode oldMode
                                   }
            MB.Completed mb' ->
              B.continue $ State s { sMinibuffer = mb' }
    SomeUIMode _ ->
      case evt of
        V.EvKey (V.KChar 'x') [V.MMeta] ->
          case sUIMode s of
            SomeMiniBuffer (MiniBuffer _) -> B.continue s0
            SomeUIMode oldMode -> B.continue $ State (s { sUIMode = SomeMiniBuffer (MiniBuffer oldMode) })
        V.EvKey (V.KChar 's') [] -> do
          liftIO (sEmitEvent s ShowSummary)
          B.continue (State s)
        V.EvKey (V.KChar 'm') [] -> do
          liftIO (sEmitEvent s ShowDiagnostics)
          B.continue (State s)
        V.EvKey (V.KChar 'q') [V.MCtrl] -> do
          liftIO (sEmitEvent s Exit)
          B.continue (State s)
        V.EvKey _k [] -> B.continue s0
        _ -> B.continue s0

appStartEvent :: State -> B.EventM Names State
appStartEvent s0 = return s0

surveyor :: Maybe FilePath -> IO ()
surveyor mExePath = do
  customEventChan <- B.newBChan 100
  let app = B.App { B.appDraw = appDraw
                  , B.appChooseCursor = appChooseCursor
                  , B.appHandleEvent = appHandleEvent
                  , B.appStartEvent = appStartEvent
                  , B.appAttrMap = appAttrMap
                  }
  _ <- T.traverse (asynchronouslyLoad customEventChan) mExePath
  let commands = [ MB.Command "summary" PL.Nil PL.Nil (\_ -> B.writeBChan customEventChan ShowSummary)
                 , MB.Command "exit" PL.Nil PL.Nil (\_ -> B.writeBChan customEventChan Exit)
                 , MB.Command "log" PL.Nil PL.Nil (\_ -> B.writeBChan customEventChan ShowDiagnostics)
                 ]
  let initialState = State S { sInputFile = mExePath
                             , sBinaryInfo = Nothing
                             , sDiagnosticLog = Seq.empty
                             , sFunctionList = (B.list FunctionList (V.empty @(FunctionListEntry 64)) 1)
                             , sUIMode = SomeUIMode Diags
                             , sAppState = maybe AwaitingFile (const Loading) mExePath
                             , sMinibuffer = MB.minibuffer MinibufferEditor MinibufferCompletionList "M-x" commands
                             , sEmitEvent = B.writeBChan customEventChan
                             }
  _finalState <- B.customMain (V.mkVty V.defaultConfig) (Just customEventChan) app initialState
  return ()

