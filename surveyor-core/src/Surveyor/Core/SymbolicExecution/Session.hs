{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Surveyor.Core.SymbolicExecution.Session (
  SessionID,
  newSessionID
  ) where

import           Control.DeepSeq ( NFData(..) )
import qualified Data.Parameterized.Nonce as PN
import qualified Prettyprinter as PP

-- | A unique identifier for a symbolic execution task (whose state is one of 'SymbolicExecutionState')
--
-- This is a wrapper around a 'PN.Nonce', but a newtype to encode that we don't
-- use the type parameter (only the state thread parameter)
newtype SessionID s = SessionID (PN.Nonce s ())
  deriving (Show, Eq, Ord)

-- | We don't have an 'NFData' instance for nonces, so we just take it to WHNF
instance NFData (SessionID s) where
  rnf (SessionID n) = n `seq` ()

instance PP.Pretty (SessionID s) where
  pretty (SessionID nonce) = PP.pretty "Session" <> PP.brackets (PP.pretty (PN.indexValue nonce))

newSessionID :: PN.NonceGenerator IO s -> IO (SessionID s)
newSessionID ng = SessionID <$> PN.freshNonce ng

