{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | This module encapsulates all of the logic for managing and rotating logs
--
-- It is currently based on the Lumberjack package.
module Surveyor.Core.Logging (
  -- * Log actions
  LogAction,
  logToFile,
  logToState,
  logMessage,
  defaultLogFile,
  -- * Log messages
  CLM.Source(..),
  CLM.Severity(..),
  CLM.LogMessage(..),
  CLM.msgWith,
  CLM.Timestamped,
  CLM.Timestamp,
  CLM.logTime,
  CLM.logMsg,
  CLM.timestamp,
  -- * Log management
  LogStore,
  takeLogs,
  appendLog
  ) where

import qualified Control.Concurrent as CC
import qualified Control.Concurrent.Async as A
import qualified Data.Sequence as Seq
import qualified Data.Text.IO as TIO
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Text.Prettyprint.Doc.Render.Text as PPT
import qualified Lumberjack as L
import           Numeric.Natural ( Natural )
import qualified System.Directory as SD
import           System.FilePath ( (</>) )
import qualified System.IO as IO

import qualified Surveyor.Core.Chan as SCC
import qualified Surveyor.Core.Events as SCE
import qualified Surveyor.Core.Logging.Message as CLM

newtype LogAction = LogAction (L.LogAction IO CLM.LogMessage)
  deriving (Monoid, Semigroup)

logMessage :: LogAction -> CLM.LogMessage -> IO ()
logMessage (LogAction a) msg = L.writeLog a msg

-- | The data structure in which in-memory logs are stored
--
-- It could be bounded by a configurable buffer size.
data LogStore =
  LogStore { logs :: !(Seq.Seq (CLM.Timestamped CLM.LogMessage))
           }

-- | Take the @n@ most recent logs
takeLogs :: Natural -> LogStore -> Seq.Seq (CLM.Timestamped CLM.LogMessage)
takeLogs n (LogStore ls) = Seq.drop (Seq.length ls - fromIntegral n) ls

instance Semigroup LogStore where
  ls1 <> ls2 = LogStore (logs ls1 <> logs ls2)

instance Monoid LogStore where
  mempty = emptyLogStore

emptyLogStore :: LogStore
emptyLogStore = LogStore { logs = mempty }

appendLog :: CLM.Timestamped CLM.LogMessage -> LogStore -> LogStore
appendLog msg st = st { logs = logs st Seq.|> msg }

-- | This logger sends a message to direct the system to add a log message to
-- the log store (for display in the UI)
logToState :: SCC.Chan (SCE.Events s st) -> LogAction
logToState chan =
  LogAction (CLM.addTimestamp logAction)
  where
    logAction = L.LogAction (\msg -> SCC.writeChan chan (SCE.LogDiagnostic msg))

-- | Create a 'LogAction' that logs to the given file
--
-- This function also returns the asynchronous task that manages reading queued
-- log messages and flushing them to disk so that the thread can be canceled if
-- the log target changes.
logToFile :: FilePath -> IO (A.Async (), LogAction)
logToFile fp = do
  c <- CC.newChan
  logTask <- A.async $ do
    IO.withFile fp IO.AppendMode $ \h ->
      writeMessageToFile h c
  let logAction = L.LogAction (CC.writeChan c)
  return (logTask, LogAction (CLM.addTimestamp logAction))

-- | Return the path of the default surveyor log file
--
-- This uses the XDG spec on Unix systems to identify a suitable user-local
-- logging directory.
defaultLogFile :: IO FilePath
defaultLogFile = do
  logDir <- SD.getXdgDirectory SD.XdgData "surveyor"
  SD.createDirectoryIfMissing True logDir
  return (logDir </> "surveyor.log")

-- | This worker runs in an asynchronous thread to flush the message queue to disk
writeMessageToFile :: PP.Pretty a => IO.Handle -> CC.Chan a -> IO ()
writeMessageToFile hdl c = do
  msg <- CC.readChan c
  let doc = PP.layoutSmart PP.defaultLayoutOptions (PP.pretty msg)
  TIO.hPutStrLn hdl (PPT.renderStrict doc)
  IO.hFlush hdl
  writeMessageToFile hdl c
