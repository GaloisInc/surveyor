{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
-- | Configuration for the symbolic execution engine
--
-- This is a separate module to break an import cycle with the Events module.
module Surveyor.Core.Context.SymbolicExecution.Config (
  SymbolicExecutionConfig(..),
  defaultSymbolicExecutionConfig,
  SomeFloatModeRepr(..),
  Solver(..),
  configSolverL,
  configFloatReprL,
  solverInteractionFileL
  ) where

import qualified Control.Lens as L
import           Data.Maybe ( isJust )
import           Data.Parameterized.Classes ( testEquality )
import qualified Data.Text as T
import qualified What4.InterpretedFloatingPoint as WIF
import qualified What4.Expr.Builder as WEB

data Solver = CVC4 | Yices | Z3
  deriving (Eq, Ord, Show)

data SomeFloatModeRepr s where
  SomeFloatModeRepr :: (forall st . WIF.IsInterpretedFloatExprBuilder (WEB.ExprBuilder s st (WEB.Flags fm)))
                    => WEB.FloatModeRepr fm
                    -> SomeFloatModeRepr s

deriving instance Show (SomeFloatModeRepr s)

instance Eq (SomeFloatModeRepr s) where
  SomeFloatModeRepr r1 == SomeFloatModeRepr r2 =
    isJust (testEquality r1 r2)

data SymbolicExecutionConfig s where
  SymbolicExecutionConfig :: (forall st . WIF.IsInterpretedFloatExprBuilder (WEB.ExprBuilder s st (WEB.Flags fm)))
                          => Solver
                          -> WEB.FloatModeRepr fm
                          -> T.Text
                          -> SymbolicExecutionConfig s

-- | a default configuration for the symbolic execution engine that uses Yices
-- and interprets floating point values as reals
defaultSymbolicExecutionConfig :: SymbolicExecutionConfig s
defaultSymbolicExecutionConfig =
  SymbolicExecutionConfig Yices WEB.FloatRealRepr ""

configSolverL :: L.Lens' (SymbolicExecutionConfig s) Solver
configSolverL f (SymbolicExecutionConfig solver fm fp) =
  fmap (\solver' -> SymbolicExecutionConfig solver' fm fp) (f solver)

configFloatReprL :: L.Lens' (SymbolicExecutionConfig s) (SomeFloatModeRepr s)
configFloatReprL f (SymbolicExecutionConfig solver fm fp) =
  fmap (\(SomeFloatModeRepr fm') -> SymbolicExecutionConfig solver fm' fp) (f (SomeFloatModeRepr fm))

solverInteractionFileL :: L.Lens' (SymbolicExecutionConfig s) T.Text
solverInteractionFileL f (SymbolicExecutionConfig solver fm fp) =
  fmap (\fp' -> SymbolicExecutionConfig solver fm fp') (f fp)
