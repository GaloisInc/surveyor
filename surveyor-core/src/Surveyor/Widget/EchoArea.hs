module Surveyor.Widget.EchoArea (
  EchoArea,
  echoArea,
  setText,
  renderEchoArea
  ) where

import qualified Brick as B
import qualified Control.Concurrent as C
import qualified Control.Concurrent.Async as A
import qualified Data.Text as T

data EchoArea =
  EchoArea { timeoutSeconds :: Int
           , killFunc :: EchoArea -> IO ()
           , content :: Maybe (T.Text, A.Async ())
           }

echoArea :: Int
         -- ^ Timeout in seconds
         -> (EchoArea -> IO ())
         -- ^ A callback provided with the new echo area after a timeout
         -> EchoArea
echoArea ts kill =
  EchoArea { timeoutSeconds = ts
           , killFunc = kill
           , content = Nothing
           }

setText :: EchoArea -> T.Text -> IO EchoArea
setText ea t = do
  case content ea of
    Just (_, oldTimeoutThread) -> do
      A.cancel oldTimeoutThread
    Nothing -> return ()
  newTimeoutThread <- A.async $ do
    C.threadDelay (timeoutSeconds ea * 1000000)
    killFunc ea (ea { content = Nothing } )
  return ea { content = Just (t, newTimeoutThread) }

renderEchoArea :: EchoArea -> B.Widget n
renderEchoArea ea = B.vLimit 1 w
  where
    w = case content ea of
      Nothing -> B.emptyWidget
      Just (txt, _) -> B.txt txt
