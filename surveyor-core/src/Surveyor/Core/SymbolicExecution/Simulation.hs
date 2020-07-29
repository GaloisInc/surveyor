{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
module Surveyor.Core.SymbolicExecution.Simulation
  ( SimulationData(..),
    breakpointP,
    modelViewP,
  ) where

import qualified Control.Lens as L

import qualified Crux.Types as CT
import qualified Surveyor.Core.Breakpoint as SCB

data SimulationData sym where
  SimBreakpoint :: !(SCB.Breakpoint sym) -> SimulationData sym
  SimModelView :: !CT.ModelView -> SimulationData sym

breakpointP :: L.Prism' (SimulationData sym) (SCB.Breakpoint sym)
breakpointP = L.prism SimBreakpoint $ \case
  SimBreakpoint bp -> Right bp
  s -> Left s

modelViewP :: L.Prism' (SimulationData sym) CT.ModelView
modelViewP = L.prism SimModelView $ \case
  SimModelView mv -> Right mv
  s -> Left s
