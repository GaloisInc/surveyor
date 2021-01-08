module Crux.Debug.Interrupt (
  installInterruptHandler
  ) where

import qualified Data.IORef as IOR

import qualified Surveyor.Core as SC

-- | Install a signal handler to let the user interrupt crux-dbg and bring up the UI
--
-- NOTE: This implementation is a no-op.  It could be implemented using
-- GHC.ConsoleHandler on Windows.
installInterruptHandler :: SC.DebuggerStateRef p sym ext -> IO ()
installInterruptHandler _ = return ()
