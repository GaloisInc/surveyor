module Surveyor ( surveyor ) where

import qualified Brick as B
import qualified Brick.BChan as B
import qualified Graphics.Vty as V

data State = State
data Names = Main
  deriving (Eq, Ord, Show)
data Events = None

appDraw :: State -> [B.Widget Names]
appDraw = undefined

appChooseCursor :: State -> [B.CursorLocation Names] -> Maybe (B.CursorLocation Names)
appChooseCursor = undefined

appHandleEvent :: State -> B.BrickEvent Names Events -> B.EventM Names (B.Next State)
appHandleEvent = undefined

appStartEvent :: State -> B.EventM Names State
appStartEvent = undefined

appAttrMap :: State -> B.AttrMap
appAttrMap = undefined

surveyor :: Maybe FilePath -> IO ()
surveyor mExePath = do
  customEventChan <- B.newBChan 100
  let app = B.App { B.appDraw = appDraw
                  , B.appChooseCursor = appChooseCursor
                  , B.appHandleEvent = appHandleEvent
                  , B.appStartEvent = appStartEvent
                  , B.appAttrMap = appAttrMap
                  }
  let initialState = State
  _finalState <- B.customMain (V.mkVty V.defaultConfig) (Just customEventChan) app initialState
  return ()
