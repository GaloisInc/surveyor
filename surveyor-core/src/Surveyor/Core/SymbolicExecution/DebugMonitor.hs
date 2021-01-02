{-# LANGUAGE GADTs #-}
-- | Helpers for monitoring a symbolic execution run for debug events that need
-- to be addressed
module Surveyor.Core.SymbolicExecution.DebugMonitor (
  debugMonitor,
  overrideMonitor,
  terminateDebugMonitor,
  terminateOverrideMonitor
  ) where

import qualified Control.Concurrent.Chan as CCC
import qualified Control.Concurrent.MVar as MV

import qualified Surveyor.Core.Chan as SCC
import qualified Surveyor.Core.Events as SCE
import qualified Surveyor.Core.SymbolicExecution.ExecutionFeature as SEEF
import qualified Surveyor.Core.SymbolicExecution.Override as SEO

-- | Run the init-once action (if it is present)
--
-- Note that it is run synchronously and thus should not block.  Since we only
-- want to run it once, a successful run of the action leaves the 'MV.MVar'
-- empty.
runInitOnce :: MV.MVar (IO ()) -> IO ()
runInitOnce mv = do
  mAction <- MV.tryTakeMVar mv
  case mAction of
    Nothing -> return ()
    Just act -> act


-- | Monitor the channel of events from the debugger execution feature.
--
-- This reads events off of the channel and converts them into Surveyor events
-- to be processed by the main event loop.
--
-- NOTE: This function is synchronous; most callers will want to run it in a
-- separate thread
--
-- NOTE: This function terminates when a 'Nothing' event is received on the
-- channel (allowing orchestration code to cleanly shut down the thread).
debugMonitor :: MV.MVar (IO ())
             -> SCC.Chan (SCE.Events s st)
             -> SEEF.DebuggerConfig s p sym arch ext
             -> IO ()
debugMonitor initOnce chan conf@(SEEF.DebuggerConfig archNonce sessionID fromFeature toFeature featureState) = do
  mstate <- CCC.readChan fromFeature
  runInitOnce initOnce
  case mstate of
    Nothing -> return ()
    Just state -> do
      SCC.writeChan chan (SCE.toEvent (SCE.DebugMonitorEvent archNonce sessionID state toFeature featureState))
      debugMonitor initOnce chan conf

-- | Monitor the channel of events from debug overrides.
--
-- This reads events off of the channel and converts them into Surveyor events
-- for the main event loop.
--
-- NOTE: This function is synchronous and intended to be run in a separate thread.
--
-- NOTE: This loop terminates when a 'Nothing' is received on the channel.
overrideMonitor :: MV.MVar (IO ())
                -> SCC.Chan (SCE.Events s st)
                -> SEEF.DebuggerConfig s p sym arch ext
                -> SEO.OverrideConfig s p sym arch ext
                -> IO ()
overrideMonitor initOnce chan debugConf conf@(SEO.OverrideConfig archNonce sessionID fromOverride toOverride) = do
  mstate <- CCC.readChan fromOverride
  runInitOnce initOnce
  case mstate of
    Nothing -> return ()
    Just state -> do
      let dconfVar = SEEF.debuggerConfigStateVar debugConf
      SCC.writeChan chan (SCE.toEvent (SCE.OverrideMonitorEvent archNonce sessionID state toOverride dconfVar))
      overrideMonitor initOnce chan debugConf conf

terminateDebugMonitor :: SEEF.DebuggerConfig s p sym arch ext -> IO ()
terminateDebugMonitor (SEEF.DebuggerConfig _ _ fromFeature _ _) =
  CCC.writeChan fromFeature Nothing

terminateOverrideMonitor :: SEO.OverrideConfig s p sym arch ext -> IO ()
terminateOverrideMonitor (SEO.OverrideConfig _ _ fromOverride _) =
  CCC.writeChan fromOverride Nothing
