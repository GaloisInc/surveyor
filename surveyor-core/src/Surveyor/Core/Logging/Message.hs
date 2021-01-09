-- | This module defines the types of messages that we log in surveyor
--
--  This is a separate module from the main logging module
--  (Surveyor.Core.Logging) to break an import cycle, as we need to refer to the
--  log message type from both Surveyor.Core.Logging and Surveyor.Core.Events.
--
--  It uses a custom log type compared to lumberjack to split out the timestamp
--  and log message into separate types with a different approach to adding
--  timestamps.
module Surveyor.Core.Logging.Message (
  Source(..),
  Severity(..),
  LogMessage(..),
  msgWith,
  msgWithContext,
  -- * Timestamp support
  Timestamped,
  logTime,
  logMsg,
  Timestamp,
  addTimestamp,
  timestamp
  ) where

import           Control.Monad.IO.Class ( MonadIO, liftIO )
import qualified Data.Functor.Contravariant as DFC
import qualified Data.Text as T
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Time.Clock as TC
import qualified Data.Time.Format as TF
import qualified GHC.Stack as GS
import qualified Lumberjack as L

data Source = Unspecified
            | EventHandler !T.Text
            | CommandCallback !T.Text
            | EchoAreaUpdate
            | Loader
  deriving (Eq, Ord, Show)

data Severity = Debug
              | Info
              | Warn
              | Error
              | Requested
              -- ^ Output explicitly requested by the user, never to be suppressed
              deriving (Eq, Ord, Show)

newtype Timestamp = Timestamp (TC.UTCTime)
  deriving (Eq, Ord, Show)

data Timestamped a =
  Timestamped { logTime :: !Timestamp
              , logMsg :: !a
              }

data LogMessage =
  LogMessage { logLevel :: !Severity
             , logSource :: !Source
             , logText :: [PP.Doc ()]
             , logContext :: GS.CallStack
             }

-- | Create a new empty log message
--
-- This is intended to be filled in by modifying the 'logLevel', 'logText', and
-- 'logSource' as necessary.
msgWith :: LogMessage
msgWith = LogMessage { logLevel = Debug
                     , logText = mempty
                     , logSource = Unspecified
                     , logContext = GS.emptyCallStack
                     }

-- | Create a new empty log message that incorporates the current calling
-- context (up to the limitations of 'GS.HasCallStack')
msgWithContext :: (GS.HasCallStack) => LogMessage
msgWithContext = msgWith { logContext = GS.callStack }

-- | A wrapper around another 'L.LogAction' that adds a timestamp
--
-- The types appear backwards because this is intended to be used with 'DFC.Contravariant'
addTimestamp :: (MonadIO m) => L.LogAction m (Timestamped msg) -> L.LogAction m msg
addTimestamp a = L.LogAction $ \msg -> do
  t <- Timestamp <$> liftIO TC.getCurrentTime
  L.writeLog (DFC.contramap (Timestamped t) a) msg

timestamp :: (MonadIO m) => msg -> m (Timestamped msg)
timestamp msg = do
  t <- Timestamp <$> liftIO TC.getCurrentTime
  return (Timestamped t msg)

prettySource :: Source -> PP.Doc ann
prettySource ctx =
  case ctx of
    Unspecified -> PP.emptyDoc
    Loader -> PP.pretty "Loader"
    EventHandler evt -> PP.pretty "Event" <> PP.brackets (PP.pretty evt)
    CommandCallback cmd -> PP.pretty "Command" <> PP.brackets (PP.pretty cmd)
    EchoAreaUpdate -> PP.pretty "Echo"

prettySeverity :: Severity -> PP.Doc ann
prettySeverity sev =
  case sev of
    Debug -> PP.pretty "Debug"
    Info -> PP.pretty "Info"
    Warn -> PP.pretty "Warn"
    Error -> PP.pretty "Error"
    Requested -> PP.emptyDoc

prettyTime :: TC.UTCTime -> PP.Doc ann
prettyTime t =
  PP.hcat [ PP.pretty (TF.formatTime TF.defaultTimeLocale "%Z-%F:%H:" t)
          , PP.pretty (TF.formatTime TF.defaultTimeLocale "%M:%S" t)
          , PP.pretty (take 4 (TF.formatTime TF.defaultTimeLocale ".%q" t))
          ]

instance PP.Pretty Timestamp where
  pretty (Timestamp ts) = prettyTime ts

prettyTimestamped :: (a -> PP.Doc ann) -> Timestamped a -> PP.Doc ann
prettyTimestamped pp ts =
  PP.hsep [ PP.pretty (logTime ts)
          , pp (logMsg ts)
          ]

prettyLogMessage :: LogMessage -> PP.Doc ann
prettyLogMessage lm =
  case logText lm of
    [] -> PP.hsep [ prettySeverity (logLevel lm)
                  , prettySource (logSource lm)
                  ]
    [msg] -> PP.hsep [ prettySeverity (logLevel lm)
                     , prettySource (logSource lm)
                     , PP.unAnnotate msg
                     ]
    msgLines ->
      let pfx = prettySeverity (logLevel lm) PP.<+> prettySource (logSource lm)
      in pfx PP.<+> PP.align (PP.unAnnotate (PP.vsep msgLines))

instance PP.Pretty LogMessage where
  pretty = prettyLogMessage

instance (PP.Pretty a) => PP.Pretty (Timestamped a) where
  pretty = prettyTimestamped PP.pretty
