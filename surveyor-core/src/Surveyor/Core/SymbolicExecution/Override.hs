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
import qualified Surveyor.Core.SymbolicExecution.Session as SCSSe
import qualified Surveyor.Core.SymbolicExecution.Simulation as SCSS

data CrucibleSimState s p sym ext where
  CrucibleSimState :: PN.Nonce s (rtp, f)
                   -> PN.Nonce s args
                   -> LCSET.SimState p sym ext rtp f args
                   -> SCSS.SimulationData sym
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
