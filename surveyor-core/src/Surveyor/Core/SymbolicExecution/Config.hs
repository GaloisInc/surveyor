{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
-- | Configuration for the symbolic execution engine
--
-- This is a separate module to break an import cycle with the Events module.
module Surveyor.Core.SymbolicExecution.Config (
  -- * Session Identifiers
  SessionID,
  newSessionID,
  -- * Top-level
  SymbolicExecutionConfig(..),
  defaultSymbolicExecutionConfig,
  -- * Solver configuration
  SomeFloatModeRepr(..),
  Solver(..),
  -- * Lenses
  configSolverL,
  configFloatReprL,
  solverInteractionFileL,
  sessionID
  ) where

import           Control.DeepSeq ( NFData(..), deepseq )
import qualified Control.Lens as L
import           Data.Maybe ( isJust )
import           Data.Parameterized.Classes ( testEquality )
import qualified Data.Parameterized.Nonce as PN
import qualified Data.Text as T
import qualified What4.Expr.Builder as WEB
import qualified What4.InterpretedFloatingPoint as WIF

import           Surveyor.Core.SymbolicExecution.Session ( SessionID, newSessionID )

data Solver = CVC4 | Yices | Z3
  deriving (Eq, Ord, Show)

instance NFData Solver where
  rnf !_ = ()

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
                          => SessionID s
                          -- | The solver to use during symbolic execution
                          -> Solver
                          -- | The floating point mode to use during symbolic execution
                          -> WEB.FloatModeRepr fm
                          -- | The solver interaction file
                          -> T.Text
                          -> SymbolicExecutionConfig s

instance NFData (SymbolicExecutionConfig s) where
  rnf (SymbolicExecutionConfig sid solver _fm t) =
    sid `deepseq` solver `deepseq` t `deepseq` ()

-- | a default configuration for the symbolic execution engine that uses Yices
-- and interprets floating point values as reals
defaultSymbolicExecutionConfig :: PN.NonceGenerator IO s -> IO (SymbolicExecutionConfig s)
defaultSymbolicExecutionConfig ng = do
  sid <- newSessionID ng
  return (SymbolicExecutionConfig sid Yices WEB.FloatRealRepr "")

configSolverL :: L.Lens' (SymbolicExecutionConfig s) Solver
configSolverL f (SymbolicExecutionConfig sid solver fm fp) =
  fmap (\solver' -> SymbolicExecutionConfig sid solver' fm fp) (f solver)

configFloatReprL :: L.Lens' (SymbolicExecutionConfig s) (SomeFloatModeRepr s)
configFloatReprL f (SymbolicExecutionConfig sid solver fm fp) =
  fmap (\(SomeFloatModeRepr fm') -> SymbolicExecutionConfig sid solver fm' fp) (f (SomeFloatModeRepr fm))

solverInteractionFileL :: L.Lens' (SymbolicExecutionConfig s) T.Text
solverInteractionFileL f (SymbolicExecutionConfig sid solver fm fp) =
  fmap (\fp' -> SymbolicExecutionConfig sid solver fm fp') (f fp)

sessionID :: L.Getter (SymbolicExecutionConfig s) (SessionID s)
sessionID = L.to (\(SymbolicExecutionConfig sid _solver _fm _fp) -> sid)
