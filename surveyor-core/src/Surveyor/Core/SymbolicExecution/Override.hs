{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeInType #-}
module Surveyor.Core.SymbolicExecution.Override (
  OverrideConfig(..),
  CrucibleSimState(..),
  ReturnSimState(..),
  newOverrideConfig
  ) where

import qualified Control.Concurrent.Chan as CCC
import qualified Data.Parameterized.Nonce as PN
import qualified Lang.Crucible.Backend as CB
import qualified Lang.Crucible.Simulator.ExecutionTree as LCSET
import qualified What4.Expr.Builder as WEB

import qualified Surveyor.Core.Architecture as SCA
import qualified Surveyor.Core.SymbolicExecution.State as SCSSt
import qualified Surveyor.Core.SymbolicExecution.Session as SCSSe

-- | This is a wrapper around all of the data generated when symbolic execution is suspended
--
-- This includes a reason for the suspension, as well as the simulator state
-- (and some nonces used to witness type equality).  This is the bundle of data
-- sent from whatever suspended execution to surveyor.
--
-- Note that in the case of a suspended symbolic execution step (from the
-- execution feature), the 'LCSET.SimState' is duplicated between this data and
-- the 'LCSET.ExecState' in the reason.
data CrucibleSimState s p sym ext where
  CrucibleSimState :: PN.Nonce s (rtp, f)
                   -> PN.Nonce s args
                   -> LCSET.SimState p sym ext rtp f args
                   -> SCSSt.SuspendedReason p sym ext rtp
                   -> CrucibleSimState s p sym ext

data ReturnSimState s p sym ext where
  UnmodifiedSimState :: ReturnSimState s p sym ext
  ModifiedSimState :: PN.Nonce s (rtp, f)
                   -> PN.Nonce s args
                   -> LCSET.SimState p sym ext rtp f args
                   -> ReturnSimState s p sym ext

data OverrideConfig s p sym arch ext where
  OverrideConfig :: ( ext ~ SCA.CrucibleExt arch
                    , sym ~ WEB.ExprBuilder s st fs
                    , CB.IsSymInterface sym
                    )
                 => PN.Nonce s arch
                 -> SCSSe.SessionID s
                 -> CCC.Chan (Maybe (CrucibleSimState s p sym ext))
                 -> CCC.Chan (ReturnSimState s p sym ext)
                 -> OverrideConfig s p sym arch ext

newOverrideConfig :: ( ext ~ SCA.CrucibleExt arch
                     , sym ~ WEB.ExprBuilder s st fs
                     , CB.IsSymInterface sym
                     )
                  => PN.Nonce s arch
                  -> SCSSe.SessionID s
                  -> IO (OverrideConfig s p sym arch ext)
newOverrideConfig archNonce sessionID = do
  c1 <- CCC.newChan
  c2 <- CCC.newChan
  return (OverrideConfig archNonce sessionID c1 c2)
