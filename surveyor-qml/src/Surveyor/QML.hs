{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Surveyor.QML ( surveyor) where

import qualified Brick.BChan as B
import qualified Control.Concurrent.Async as A
import qualified Control.Concurrent.MVar as MV
import           Control.Lens ( (&), (^.), (^?), (.~), _Just )
import           Control.Monad ( forever )
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Nonce as PN
import qualified Data.Parameterized.List as PL
import           Data.Parameterized.Some ( Some(..) )
import           Data.Proxy ( Proxy(..) )
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Traversable as T
import           Data.Void ( Void )
import qualified Graphics.QML as Q
import qualified System.Exit as IO

import qualified Surveyor.Architecture as A
import qualified Surveyor.Arguments as AR
import qualified Surveyor.Commands as C
import qualified Surveyor.Core.Command as C
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
      klass <- defineContextClass
      ctxObj <- Q.newObject klass o
      hdlrThread <- A.async $ forever $ handleEvents customEventChan ctxObj
      guiThread <- A.asyncBound $ do
        let cfg = Q.defaultEngineConfig { Q.initialDocument = Q.fileDocument uiFile
                                        , Q.contextObject = Just (Q.anyObjRef ctxObj)
                                        }
        Q.runEngineLoop cfg
      eres <- A.waitCatch hdlrThread
      case eres of
        Left err -> do
          print err
          A.cancel guiThread
        Right _ -> return ()
  return ()

handleEvents :: B.BChan (E.Events PN.GlobalNonceGenerator) -> Q.ObjRef Context -> IO ()
handleEvents chan ref = do
  let mv = Q.fromObjRef ref
  e <- B.readBChan chan
  case e of
    E.AnalysisFinished (A.SomeResult ar) _ -> updateArchRes mv ar
    E.AnalysisProgress (A.SomeResult ar) -> updateArchRes mv ar
    E.Exit -> do
      Q.fireSignal (Proxy @ShutdownSignal) ref
      IO.exitSuccess

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
            , sUIState = QMLUIState { sMinibuffer = Minibuffer C.allCommands
                                    }
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

data Minibuffer e st arch s =
  Minibuffer { mbCommands :: [Some (C.Command e st (AR.Argument arch e st s) AR.TypeRepr)]
             }

data QMLUIState arch s =
  QMLUIState { sMinibuffer :: Minibuffer (E.Events s) (Maybe (PN.Nonce s arch)) arch s
             }

defineContextClass :: IO (Q.Class Context)
defineContextClass = do
  Q.newClass [ Q.defMethod' "hello" hello
             , Q.defMethod' "world" world
             , Q.defMethod' "bye" bye
             , Q.defMethod' "runCommand" runCommand
             , Q.defSignal "shutdown" (Proxy @ShutdownSignal)
             ]

data ShutdownSignal

instance Q.SignalKeyClass ShutdownSignal where
  type SignalParams ShutdownSignal = IO ()

runCommand :: Q.ObjRef Context -> T.Text -> IO ()
runCommand r cmdString = do
  print cmdString
  State s <- MV.readMVar (Q.fromObjRef r)
  case s ^? lArchState . _Just . lUIState of
    Nothing -> putStrLn "No state" >> return ()
    Just uis -> do
      let cmds = mbCommands (sMinibuffer uis)
      case matchCommand cmdString cmds of
        Nothing -> putStrLn "No command" >> return ()
        Just (Some cmd) ->
          case C.cmdArgTypes cmd of
            PL.Nil -> do
              putStrLn "Ran command"
              C.cmdFunc cmd (s ^. lEventChannel) (s ^? lArchState . _Just . lNonce) PL.Nil
            _ -> putStrLn "Argument shape" >> return ()

matchCommand :: T.Text
             -> [Some (C.Command e st (AR.Argument arch e st s) AR.TypeRepr)]
             -> Maybe (Some (C.Command e st (AR.Argument arch e st s) AR.TypeRepr))
matchCommand s cmds =
  case cmds of
    [] -> Nothing
    Some cmd : rest
      | C.cmdName cmd == s -> Just (Some cmd)
      | otherwise -> matchCommand s rest

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
