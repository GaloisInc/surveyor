-- | This module defines an interface around channels
--
-- Different frontends may have dependencies on different channel types.  We use
-- this abstraction so that the core doesn't need to be tied to one particular
-- channel implementation.  The frontends can wrap their respective channels
-- with this interface.
module Surveyor.Chan (
  Chan,
  mkChan,
  readChan,
  writeChan
  ) where

data Chan a =
  Chan { readChan :: IO a
       , writeChan :: a -> IO ()
       }

mkChan :: IO a -> (a -> IO ()) -> Chan a
mkChan = Chan

