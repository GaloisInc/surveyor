module Crux.Debug.Interrupt (
  installInterruptHandler
  ) where

import qualified System.Posix.Signals as SPS

import qualified Surveyor.Core as SC

-- | Install a handler that responds to SIGUSR2 by setting the debug execution
-- feature to 'Monitoring' mode so that the next step will interrupt (and bring
-- up the debugger).
--
-- This only needs to change the execution feature mode, because the first
-- message sent from the debug execution feature in monitor mode will initialize
-- the UI
--
-- NOTE: The handler will only fire once (to bring up the UI).  If the user
-- closes the UI, we would need to reinstall the handler... this is to avoid
-- multiple UI instantiations at once.  To really make this robust, we might
-- need a bit of a redesign to the init-once UI initialization (to reset it).
--
-- NOTE: This is currently UNIX only.  Another approach (using
-- GHC.ConsoleHandler) will be required for Windows.
installInterruptHandler :: SC.DebuggerStateRef p sym ext -> IO ()
installInterruptHandler ref = do
  _ <- SPS.installHandler SPS.sigUSR2 (SPS.CatchOnce handler) Nothing
  return ()
  where
    handler = SC.setDebuggerState ref SC.Monitoring
