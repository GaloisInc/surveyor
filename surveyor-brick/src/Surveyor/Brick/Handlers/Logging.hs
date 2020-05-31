{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Surveyor.Brick.Handlers.Logging ( handleLoggingEvent ) where

import qualified Brick as B
import qualified Control.Concurrent.Async as A
import           Control.Lens ( (&), (^.), (.~), (%~) )
import           Control.Monad.IO.Class ( liftIO )
import qualified Data.Text as T
import           Surveyor.Brick.Names ( Names(..) )
import qualified Surveyor.Core as C

import qualified Surveyor.Brick.Extension as SBE

handleLoggingEvent :: (C.Architecture arch s)
                   => C.S SBE.BrickUIExtension SBE.BrickUIState arch s
                   -> C.LoggingEvent s (C.S SBE.BrickUIExtension SBE.BrickUIState)
                   -> B.EventM Names (B.Next (C.State SBE.BrickUIExtension SBE.BrickUIState s))
handleLoggingEvent s0 evt =
  case evt of
    C.LogDiagnostic msg
      | C.logLevel (C.logMsg msg) >= C.sDiagnosticLevel s0 ->
        B.continue $! C.State (s0 & C.lLogStore %~ C.appendLog msg)
      | otherwise -> B.continue $! C.State s0
    C.SetLogFile fp -> do
      dfltFile <- liftIO C.defaultLogFile
      let logFile = if null fp then dfltFile else fp
      (newTask, newAction) <- liftIO $ C.logToFile logFile
      case s0 ^. C.lLogActions . C.lFileLogger of
        Nothing -> return ()
        Just (task, _) -> liftIO $ A.cancel task
      let s1 = s0 & C.lLogActions . C.lFileLogger .~ Just (newTask, newAction)
      liftIO $ C.logMessage s1 (C.msgWith { C.logLevel = C.Info
                                          , C.logText = ["Logging to file " <> T.pack logFile]
                                          , C.logSource = C.EventHandler "SetLogFile"
                                          })
      B.continue $! C.State s1
    C.DisableFileLogging -> do
      case s0 ^. C.lLogActions . C.lFileLogger of
        Nothing -> B.continue $! C.State s0
        Just (task, _) -> do
          liftIO $ A.cancel task
          let s1 = s0 & C.lLogActions . C.lFileLogger .~ Nothing
          B.continue $! C.State s1
