{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Surveyor.Core.Breakpoint (
  Breakpoint(..),
  BreakpointType(..),
  classifyBreakpoint
  ) where

import           Control.Lens ( (^.) )
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Text as T
import qualified Data.Vector as DV

import qualified Lang.Crucible.Backend as LCB
import qualified Lang.Crucible.Simulator.CallFrame as LCSC
import qualified Lang.Crucible.Simulator.ExecutionTree as LCSET
import qualified Lang.Crucible.Simulator.RegMap as CSR
import qualified Lang.Crucible.Simulator.RegValue as LCSR
import qualified Lang.Crucible.Types as LCT

import qualified Surveyor.Core.Architecture.Class as SCAC

data BreakpointType sym = UnconditionalBreakpoint
                        | ConditionalBreakpoint (LCSR.RegValue sym LCT.BoolType)

data Breakpoint sym =
  Breakpoint { breakpointType :: BreakpointType sym
             , breakpointName :: Maybe T.Text
             , breakpointArguments :: DV.Vector (LCSR.RegValue sym LCT.AnyType)
             }

-- | Classify a call as a known breakpoint (or not)
--
-- All of our breakpoints are overrides with distinguished name
--
-- The first argument of the (unconditional) breakpoint function is the name.
-- The rest of the arguments are values that can be viewed easily from the
-- debugger.
classifyBreakpoint :: forall proxy arch s sym p ext ret rtp f a
                    . ( SCAC.SymbolicArchitecture arch s
                      , p ~ SCAC.CruciblePersonality arch sym
                      , ext ~ SCAC.CrucibleExt arch
                      , LCB.IsSymInterface sym
                      )
                   => proxy (arch, s)
                   -> LCSET.SimState p sym ext rtp f a
                   -> LCSET.ResolvedCall p sym ext ret
                   -> Maybe (IO (Breakpoint sym))
classifyBreakpoint proxy simState rc =
  case rc of
    LCSET.CrucibleCall {} -> Nothing
    LCSET.OverrideCall o fr
      | LCSET.overrideName o == "crucible_breakpoint" ->
        case CSR.regMap (fr ^. LCSC.overrideRegMap) of
          Ctx.Empty Ctx.:> name Ctx.:> args
            | LCT.VectorRepr LCT.AnyRepr <- CSR.regType args -> Just $ do
                mName <- SCAC.loadConcreteString proxy simState (CSR.regType name) (CSR.regValue name)
                return Breakpoint { breakpointType = UnconditionalBreakpoint
                                  , breakpointName = mName
                                  , breakpointArguments = CSR.regValue args
                                  }
          _ -> Nothing
      | LCSET.overrideName o == "crucible_breakpoint_if" ->
        case CSR.regMap (fr ^. LCSC.overrideRegMap) of
          Ctx.Empty Ctx.:> name Ctx.:> predicate Ctx.:> args
            | LCT.VectorRepr LCT.AnyRepr <- CSR.regType args
            , LCT.BoolRepr <- CSR.regType predicate -> Just $ do
                mName <- SCAC.loadConcreteString proxy simState (CSR.regType name) (CSR.regValue name)
                return Breakpoint { breakpointType = ConditionalBreakpoint (CSR.regValue predicate)
                                  , breakpointName = mName
                                  , breakpointArguments = CSR.regValue args
                                  }
          _ -> Nothing
      | otherwise -> Nothing
