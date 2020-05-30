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

logToState :: SCC.Chan (SCE.Events s st) -> LogAction
logToState chan =
  LogAction (CLM.addTimestamp logAction)
  where
    logAction = L.LogAction (\msg -> SCC.writeChan chan (SCE.LogDiagnostic msg))

logToFile :: FilePath -> IO LogAction
logToFile fp = do
  c <- CC.newChan
  _ <- A.async $ do
    IO.withFile fp IO.AppendMode $ \h ->
      writeMessageToFile h c
  let logAction = L.LogAction (CC.writeChan c)
  return (LogAction (CLM.addTimestamp logAction))

writeMessageToFile :: PP.Pretty a => IO.Handle -> CC.Chan a -> IO ()
writeMessageToFile hdl c = do
  msg <- CC.readChan c
  let doc = PP.layoutSmart PP.defaultLayoutOptions (PP.pretty msg)
  TIO.hPutStrLn hdl (PPT.renderStrict doc)
  writeMessageToFile hdl c
