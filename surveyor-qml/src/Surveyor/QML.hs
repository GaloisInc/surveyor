{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Surveyor.QML ( surveyor) where

import qualified Brick.BChan as B
import qualified Control.Concurrent.Async as A
import qualified Control.Concurrent.MVar as MV
import           Control.Lens ( (&), (^.), (^?), (.~), _Just )
import           Control.Monad ( forever )
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Nonce as PN
import qualified Data.Sequence as Seq
import qualified Data.Traversable as T
import           Data.Void ( Void )
import qualified Graphics.QML as Q

import qualified Surveyor.Architecture as A
import           Surveyor.Core.State
import qualified Surveyor.Events as E
import qualified Surveyor.Keymap as K
import qualified Surveyor.Mode as SM
import           Surveyor.Loader ( AsyncLoader, asynchronouslyLoad )
import qualified Surveyor.Widget.EchoArea as EA

surveyor :: Maybe FilePath
         -> Maybe FilePath
         -> IO ()
surveyor mInitialInput mUIFile = do
  let ng = PN.globalNonceGenerator
  customEventChan <- B.newBChan 100
  mloader <- T.traverse (asynchronouslyLoad ng customEventChan) mInitialInput
  case mUIFile of
    Nothing -> error "No UI file"
    Just uiFile -> do
      o <- MV.newMVar (State (emptyState mInitialInput mloader ng customEventChan))
      hdlr <- A.async $ forever $ handleEvents customEventChan o
      A.link hdlr
      klass <- defineContextClass
      ctxObj <- Q.newObject klass o
      let cfg = Q.defaultEngineConfig { Q.initialDocument = Q.fileDocument uiFile
                                      , Q.contextObject = Just (Q.anyObjRef ctxObj)
                                      }
      Q.runEngineLoop cfg
  return ()

handleEvents :: B.BChan (E.Events PN.GlobalNonceGenerator) -> Context -> IO ()
handleEvents chan mv = do
  e <- B.readBChan chan
  case e of
    E.AnalysisFinished (A.SomeResult ar) _ -> updateArchRes mv ar
    E.AnalysisProgress (A.SomeResult ar) -> updateArchRes mv ar

updateArchRes :: (A.Architecture arch s)
              => MV.MVar (State QMLUIState s)
              -> A.AnalysisResult arch s
              -> IO ()
updateArchRes mv ar = do
  State s <- MV.takeMVar mv
  case () of
    _ | Just nonce0 <- s ^? lArchState . _Just . lNonce
      , Just Refl <- testEquality (A.archNonce ar) nonce0 ->
        MV.putMVar mv (State (s & lArchState . _Just . lAnalysisResult .~ ar))
      | otherwise -> MV.putMVar mv (State (freshState s ar))

freshState :: (A.Architecture arch1 s)
           => S QMLUIState arch2 s
           -> A.AnalysisResult arch1 s
           -> S QMLUIState arch1 s
freshState s ar = S { sInputFile = sInputFile s
                    , sLoader = sLoader s
                    , sDiagnosticLog = sDiagnosticLog s
                    , sAppState = sAppState s
                    , sUIMode = sUIMode s
                    , sEmitEvent = sEmitEvent s
                    , sEventChannel = sEventChannel s
                    , sNonceGenerator = sNonceGenerator s
                    , sEchoArea = sEchoArea s
                    , sArchState = Just (freshArchState ar)
                    }

freshArchState :: (A.Architecture arch s)
               => A.AnalysisResult arch s
               -> ArchState QMLUIState arch s
freshArchState ar =
  ArchState { sNonce = A.archNonce ar
            , sAnalysisResult = ar
            , sKeymap = K.defaultKeymap
            , sUIState = QMLUIState
            }

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

type Context =
  MV.MVar (State QMLUIState PN.GlobalNonceGenerator)

data QMLUIState arch s =
  QMLUIState

defineContextClass :: IO (Q.Class Context)
defineContextClass = do
  Q.newClass [ Q.defMethod' "hello" hello
             , Q.defMethod' "world" world
             , Q.defMethod' "bye" bye
             ]

bye :: Q.ObjRef Context -> IO ()
bye _r = do
  print "bye"
  return ()


world :: Q.ObjRef Context -> IO ()
world _r = do
  print "world"
  return ()

hello :: Q.ObjRef Context -> IO ()
hello _r = do
  print "hello"
  return ()

updateEchoArea :: B.BChan (E.Events s) -> EA.EchoArea -> IO ()
updateEchoArea customEventChan ea =
  B.writeBChan customEventChan (E.UpdateEchoArea ea)
