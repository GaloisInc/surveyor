-- | This module defines combinators for asynchronously executing IO actions on
-- worker threads and updating the application state afterwards.  Its goal is to
-- ensure that asynchronous results are fully evaluated to normal form on the
-- worker thread so that the event handling thread never needs to block.
module Surveyor.Core.Async (
  asynchronously
  ) where

import qualified Control.Concurrent.Async as A
import           Control.DeepSeq ( NFData )
import           Control.Monad ( void )
import qualified Control.Once as O
import qualified Data.Parameterized.Nonce as PN
import qualified Surveyor.Core.Events as CE

asynchronously :: (NFData a)
               => PN.Nonce s arch
               -- ^ The current nonce
               -> (CE.Events s st -> IO ())
               -- ^ A function to send an event
               -> (a -> st arch s -> st arch s)
               -- ^ A state update function
               -> IO a
               -- ^ An action that should be run in another thread
               -> IO ()
asynchronously nonce emit upd act = do
  void $ A.async $ do
    res <- act
    emit $! CE.AsyncStateUpdate nonce (O.once res) upd
