module Surveyor.QML ( surveyor) where

import qualified Brick.BChan as B
import qualified Data.Parameterized.Nonce as PN
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Sequence as Seq
import qualified Data.Traversable as T
import           Data.Void ( Void )
import qualified Graphics.QML as Q

import           Surveyor.Core.State
import qualified Surveyor.Events as E
import qualified Surveyor.Mode as SM
import           Surveyor.Loader ( AsyncLoader, asynchronouslyLoad )
import qualified Surveyor.Widget.EchoArea as EA

surveyor :: Maybe FilePath
         -> Maybe FilePath
         -> IO ()
surveyor mInitialInput mUIFile = PN.withIONonceGenerator $ \ng -> do
  customEventChan <- B.newBChan 100
  mloader <- T.traverse (asynchronouslyLoad ng customEventChan) mInitialInput
  case mUIFile of
    Nothing -> error "No UI file"
    Just uiFile -> do
      klass <- defineContextClass
      ctxObj <- Q.newObject klass (Some (State (emptyState mInitialInput mloader ng customEventChan)))
      let cfg = Q.defaultEngineConfig { Q.initialDocument = Q.fileDocument uiFile
                                      , Q.contextObject = Just (Q.anyObjRef ctxObj)
                                      }
      Q.runEngineLoop cfg
  return ()

emptyState :: Maybe FilePath
           -> Maybe AsyncLoader
           -> PN.NonceGenerator IO s
           -> B.BChan (E.Events s)
           -> S QMLUIState Void s
emptyState mInitialInput mloader ng customEventChan =
  S { sInputFile = mInitialInput
    , sLoader = mloader
    , sDiagnosticLog = Seq.empty
    , sAppState = Loading
    , sUIMode = SM.SomeUIMode SM.Summary
    , sEmitEvent = B.writeBChan customEventChan
    , sEventChannel = customEventChan
    , sNonceGenerator = ng
    , sEchoArea = EA.echoArea 10 (updateEchoArea customEventChan)
    , sArchState = Nothing
    }

data QMLUIState arch s =
  QMLUIState

defineContextClass :: IO (Q.Class (Some (State QMLUIState)))
defineContextClass = do
  Q.newClass [Q.defMethod' "hello" hello]

hello :: Q.ObjRef (Some (State QMLUIState)) -> IO ()
hello r = do
  print "hello"
  return ()

updateEchoArea :: B.BChan (E.Events s) -> EA.EchoArea -> IO ()
updateEchoArea customEventChan ea =
  B.writeBChan customEventChan (E.UpdateEchoArea ea)
