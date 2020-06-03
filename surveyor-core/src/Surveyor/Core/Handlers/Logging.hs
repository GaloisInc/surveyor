{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Surveyor.Core.Handlers.Logging ( handleLoggingEvent ) where

import qualified Control.Concurrent.Async as A
import           Control.Lens ( (&), (^.), (.~), (%~) )
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import qualified Data.Text as T

import qualified Surveyor.Core.Architecture as SCA
import qualified Surveyor.Core.Events as SCE
import qualified Surveyor.Core.Logging as SCL
import qualified Surveyor.Core.State as SCS

handleLoggingEvent :: (SCA.Architecture arch s, MonadIO m)
                   => SCS.S e u arch s
                   -> SCE.LoggingEvent s (SCS.S e u)
                   -> m (SCS.State e u s)
handleLoggingEvent s0 evt =
  case evt of
    SCE.LogDiagnostic msg
      | SCL.logLevel (SCL.logMsg msg) >= SCS.sDiagnosticLevel s0 ->
        return $! SCS.State (s0 & SCS.lLogStore %~ SCL.appendLog msg)
      | otherwise -> return $! SCS.State s0
    SCE.SetLogFile fp -> do
      dfltFile <- liftIO SCL.defaultLogFile
      let logFile = if null fp then dfltFile else fp
      (newTask, newAction) <- liftIO $ SCL.logToFile logFile
      case s0 ^. SCS.lLogActions . SCS.lFileLogger of
        Nothing -> return ()
        Just (task, _) -> liftIO $ A.cancel task
      let s1 = s0 & SCS.lLogActions . SCS.lFileLogger .~ Just (newTask, newAction)
      liftIO $ SCS.logMessage s1 (SCL.msgWith { SCL.logLevel = SCL.Info
                                              , SCL.logText = ["Logging to file " <> T.pack logFile]
                                              , SCL.logSource = SCL.EventHandler "SetLogFile"
                                              })
      return $! SCS.State s1
    SCE.DisableFileLogging -> do
      case s0 ^. SCS.lLogActions . SCS.lFileLogger of
        Nothing -> return $! SCS.State s0
        Just (task, _) -> do
          liftIO $ A.cancel task
          let s1 = s0 & SCS.lLogActions . SCS.lFileLogger .~ Nothing
          return $! SCS.State s1
