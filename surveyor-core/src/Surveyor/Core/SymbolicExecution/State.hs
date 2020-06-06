{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
module Surveyor.Core.SymbolicExecution.State (
  SymbolicState(..)
  ) where

import qualified Data.Parameterized.Context as Ctx
import qualified Lang.Crucible.Backend as CB
import qualified Lang.Crucible.CFG.Core as CCC
import qualified Lang.Crucible.Simulator as CS

import qualified Surveyor.Core.Architecture as CA
import           Surveyor.Core.SymbolicExecution.Config

data SymbolicState arch s sym init reg =
  SymbolicState { symbolicConfig :: SymbolicExecutionConfig s
                , symbolicBackend :: sym
                , someCFG :: CCC.SomeCFG (CA.CrucibleExt arch) init reg
                , symbolicRegs :: Ctx.Assignment (CS.RegEntry sym) init
                , symbolicGlobals :: CS.SymGlobalState sym
                , withSymConstraints :: forall a . (CB.IsSymInterface sym) => a -> a
                }
