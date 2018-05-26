module Surveyor.EchoArea (
  EchoArea,
  echoArea,
  resetEchoArea,
  getText,
  setText
  ) where

import qualified Control.Concurrent as C
import qualified Control.Concurrent.Async as A
import qualified Data.Text as T

data EchoArea =
  EchoArea { timeoutSeconds :: Int
           , killFunc :: IO ()
           , content :: Maybe (T.Text, A.Async ())
           }

echoArea :: Int
         -- ^ Timeout in seconds
         -> IO ()
         -- ^ A callback that resets the echo area after a timeout
         -> EchoArea
echoArea ts kill =
  EchoArea { timeoutSeconds = ts
           , killFunc = kill
           , content = Nothing
           }

getText :: EchoArea -> Maybe T.Text
getText = fmap fst . content

setText :: EchoArea -> T.Text -> IO EchoArea
setText ea t = do
  case content ea of
    Just (_, oldTimeoutThread) -> do
      A.cancel oldTimeoutThread
    Nothing -> return ()
  newTimeoutThread <- A.async $ do
    C.threadDelay (timeoutSeconds ea * 1000000)
    killFunc ea
  return ea { content = Just (t, newTimeoutThread) }

resetEchoArea :: EchoArea -> EchoArea
resetEchoArea ea = ea { content = Nothing }
