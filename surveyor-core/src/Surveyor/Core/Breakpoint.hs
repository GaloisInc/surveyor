{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Surveyor.Core.Breakpoint (
  Breakpoint(..),
  BreakpointType(..)
  ) where

import qualified Data.Text as T
import qualified Data.Vector as DV

import qualified Lang.Crucible.Simulator.RegValue as LCSR
import qualified Lang.Crucible.Types as LCT

data BreakpointType sym = UnconditionalBreakpoint
                        | ConditionalBreakpoint (LCSR.RegValue sym LCT.BoolType)

data Breakpoint sym =
  Breakpoint { breakpointType :: BreakpointType sym
             , breakpointName :: Maybe T.Text
             , breakpointArguments :: DV.Vector (LCSR.RegValue sym LCT.AnyType)
             }

