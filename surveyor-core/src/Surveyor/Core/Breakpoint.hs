{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Surveyor.Core.Breakpoint (
  Breakpoint(..),
  BreakpointType(..),
  classifyBreakpoint
  ) where

import           Control.Lens ( (^.) )
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Text as T
import qualified Data.Vector as DV

import qualified Lang.Crucible.Simulator.CallFrame as LCSC
import qualified Lang.Crucible.Simulator.ExecutionTree as LCSET
import qualified Lang.Crucible.Simulator.RegMap as CSR
import qualified Lang.Crucible.Simulator.RegValue as LCSR
import qualified Lang.Crucible.Types as LCT

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
--
-- FIXME: Interpreting the string here is a bit tricky.  In the LLVM frontend,
-- it is represented as an LLVMPointer intrinsic type.  We can't mention that
-- type here because the architecture and extension types are polymorphic.
-- We'll need a typeclass method (probably based on arch) that lets us dig into
-- arch-specific types.  Additionally, that function will need access to the
-- full SimState, as loading a string from memory in LLVM requires reading from
-- memory.
classifyBreakpoint :: LCSET.ResolvedCall p sym ext ret -> Maybe (Breakpoint sym)
classifyBreakpoint rc =
  case rc of
    LCSET.CrucibleCall {} -> Nothing
    LCSET.OverrideCall o fr
      | LCSET.overrideName o == "crucible_breakpoint" ->
        case CSR.regMap (fr ^. LCSC.overrideRegMap) of
          Ctx.Empty Ctx.:> name Ctx.:> args
            | LCT.VectorRepr LCT.AnyRepr <- CSR.regType args ->
              Just $ Breakpoint { breakpointType = UnconditionalBreakpoint
                                , breakpointName = Nothing
                                , breakpointArguments = CSR.regValue args
                                }
          _ -> Nothing
      | LCSET.overrideName o == "crucible_breakpoint_if" ->
        case CSR.regMap (fr ^. LCSC.overrideRegMap) of
          Ctx.Empty Ctx.:> name Ctx.:> predicate Ctx.:> args
            | LCT.VectorRepr LCT.AnyRepr <- CSR.regType args
            , LCT.BoolRepr <- CSR.regType predicate ->
              Just $ Breakpoint { breakpointType = ConditionalBreakpoint (CSR.regValue predicate)
                                , breakpointName = Nothing
                                , breakpointArguments = CSR.regValue args
                                }
          _ -> Nothing
      | otherwise -> Nothing
