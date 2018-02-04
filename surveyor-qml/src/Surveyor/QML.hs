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
      snapKlass <- defineSnapshotClass
      ctxKlass <- defineContextClass snapKlass
      ctxObj <- Q.newObject ctxKlass o
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
          Q.fireSignal (Proxy @ShutdownSignal) ctxObj
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

    E.ShowSummary -> do
      State s <- MV.takeMVar mv
      MV.putMVar mv (State (s & lUIMode .~ SM.SomeUIMode SM.Summary))
      ix <- getStackIndex ref
      Q.fireSignal (Proxy @UpdateStackIndex) ref ix
    E.ShowDiagnostics -> do
      State s <- MV.takeMVar mv
      MV.putMVar mv (State (s & lUIMode .~ SM.SomeUIMode SM.Diags))
      ix <- getStackIndex ref
      Q.fireSignal (Proxy @UpdateStackIndex) ref ix

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

defineContextClass :: Q.Class (State QMLUIState PN.GlobalNonceGenerator) -> IO (Q.Class Context)
defineContextClass snapshotClass =
  Q.newClass [ Q.defMethod' "runCommand" runCommand
             , Q.defMethod' "snapshotState" (snapshotState snapshotClass)
             , Q.defSignal "shutdown" (Proxy @ShutdownSignal)
             , Q.defSignal "updateStackIndex" (Proxy @UpdateStackIndex)
             ]

defineSnapshotClass :: IO (Q.Class (State QMLUIState PN.GlobalNonceGenerator))
defineSnapshotClass = Q.newClass []

getStackIndex :: Q.ObjRef Context -> IO Int
getStackIndex r = do
  State s <- MV.readMVar (Q.fromObjRef r)
  case sUIMode s of
    SM.SomeUIMode uim ->
      case uim of
        SM.Diags -> return 4
        SM.Summary -> return 0
        SM.FunctionSelector -> return 1
        SM.FunctionViewer -> return 2

-- | From the context class, take a snapshot of the state of the context object
--
-- We want this so that callbacks from QML can look at a constant object state
-- without having to worry about the value in the underlying MVar (which guards
-- the canonical state) changing out from under them.  This will let the QML
-- side issue a sequence of queries against a state snapshot.
--
-- This approach will also reduce contention for the MVar, as calls from QML to
-- Haskell will only have to synchronize once (while taking this snapshot)
snapshotState :: Q.Class (State QMLUIState PN.GlobalNonceGenerator)
              ->  Q.ObjRef Context
              -> IO (Q.ObjRef (State QMLUIState PN.GlobalNonceGenerator))
snapshotState klass ref = do
  s <- MV.readMVar (Q.fromObjRef ref)
  Q.newObject klass s

data ShutdownSignal
data UpdateStackIndex

instance Q.SignalKeyClass ShutdownSignal where
  type SignalParams ShutdownSignal = IO ()

instance Q.SignalKeyClass UpdateStackIndex where
  type SignalParams UpdateStackIndex = Int -> IO ()

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

updateEchoArea :: B.BChan (E.Events s) -> EA.EchoArea -> IO ()
updateEchoArea customEventChan ea =
  B.writeBChan customEventChan (E.UpdateEchoArea ea)
