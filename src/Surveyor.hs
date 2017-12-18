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
import           Data.Monoid
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Traversable as T
import qualified Graphics.Vty as V

import           Surveyor.BinaryAnalysisResult ( BinaryAnalysisResult(..) )
import           Surveyor.Events ( Events(..) )
import           Surveyor.Loader ( asynchronouslyLoad )

data State =
  State { sInputFile :: Maybe FilePath
        , sBinaryInfo :: Maybe BinaryAnalysisResult
        , sDiagnosticLog :: Seq.Seq T.Text
        }

data Names = Main
  deriving (Eq, Ord, Show)


appDraw :: State -> [B.Widget Names]
appDraw s =
  case sInputFile s of
    Nothing -> [B.txt "No file loaded"]
    Just binFileName ->
      case sBinaryInfo s of
        Nothing -> [B.str ("Analyzing " ++ binFileName)]
        Just _ -> [B.txt "Finished analysis"]

appChooseCursor :: State -> [B.CursorLocation Names] -> Maybe (B.CursorLocation Names)
appChooseCursor _ _ = Nothing

appHandleEvent :: State -> B.BrickEvent Names Events -> B.EventM Names (B.Next State)
appHandleEvent s0 evt =
  case evt of
    B.AppEvent ae ->
      case ae of
        AnalysisFinished bar diags ->
          let newDiags = map (\d -> T.pack ("Analysis: " ++ show d)) diags
          in B.continue $ s0 { sBinaryInfo = Just bar
                             , sDiagnosticLog = sDiagnosticLog s0 <> Seq.fromList newDiags
                             }
        AnalysisFailure exn ->
          B.continue $ s0 { sDiagnosticLog = sDiagnosticLog s0 Seq.|> T.pack ("Analysis failure: " ++ show exn) }
        _ -> B.continue s0
    B.VtyEvent vtyEvt -> handleVtyEvent s0 vtyEvt
    B.MouseDown {} -> B.continue s0
    B.MouseUp {} -> B.continue s0

handleVtyEvent :: State -> V.Event -> B.EventM Names (B.Next State)
handleVtyEvent s0 evt =
  case evt of
    V.EvKey (V.KChar 'q') [] -> B.halt s0
    V.EvKey V.KEsc [] -> B.halt s0
    _ -> B.continue s0

appStartEvent :: State -> B.EventM Names State
appStartEvent s0 = return s0

appAttrMap :: State -> B.AttrMap
appAttrMap _ = B.attrMap V.defAttr []

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
  let initialState = State { sInputFile = mExePath
                           , sBinaryInfo = Nothing
                           , sDiagnosticLog = Seq.empty
                           }
  _finalState <- B.customMain (V.mkVty V.defaultConfig) (Just customEventChan) app initialState
  return ()

