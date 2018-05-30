{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Surveyor.Core.Architecture.Class (
  Architecture(..),
  SomeResult(..),
  ResultIndex(..),
  AnalysisResult(..),
  ParameterizedFormula(..),
  prettyParameterizedFormula,
  Block(..),
  FunctionHandle(..),
  ArchConstraints
  ) where

import           GHC.Generics ( Generic )

import           Control.DeepSeq ( NFData, rnf, deepseq )
import qualified Control.Once as O
import qualified Data.Parameterized.Nonce as NG
import qualified Data.Text as T

import qualified SemMC.Architecture as SA
import qualified SemMC.Formula as F
import qualified Lang.Crucible.Backend.Simple as SB

data SomeResult s arch where
  SomeResult :: (Architecture arch s) => AnalysisResult arch s -> SomeResult s arch

instance NFData (SomeResult s arch) where
  rnf (SomeResult ar) = ar `deepseq` ()

-- | The 'ResultIndex' is a type that we use to pre-compute as much as we can
-- when we produce an analysis result (i.e., on another thread) so that we can
-- start consulting it from the event handler thread without incurring a huge
-- cost.
data ResultIndex arch s =
  ResultIndex { riFunctions :: [FunctionHandle arch s]
              , riSummary :: [(T.Text, T.Text)]
              }
  deriving (Generic)

instance (ArchConstraints arch s) => NFData (ResultIndex arch s)

data AnalysisResult arch s =
  AnalysisResult { archResult :: ArchResult arch s
                 , resultIndex :: !(O.Once (ResultIndex arch s))
                 }

-- We only force the 'ResultIndex', as we can't really force the 'ArchResult'
-- due to missing instances (and likely too much work)
instance NFData (AnalysisResult arch s) where
  rnf (AnalysisResult _ idx) = idx `deepseq` ()

type ArchConstraints arch s = (Eq (Address arch s),
                               Ord (Address arch s),
                               Show (Address arch s),
                               NFData (Address arch s),
                               NFData (Instruction arch s))

class (ArchConstraints arch s) => Architecture (arch :: *) (s :: *) where
  data ArchResult arch s :: *
  data Instruction arch s :: *
  data Operand arch s :: *
  data Opcode arch s :: *
  data Address arch s :: *

  -- | Extract the nonce for the analysis result
  archNonce :: AnalysisResult arch s -> NG.Nonce s arch
  -- | Return summary information for the artifact being analyzed; the
  -- information can vary by architecture
  summarizeResult :: AnalysisResult arch s -> [(T.Text, T.Text)]
  -- | Parse a string into an architecture-specific 'Address'
  parseAddress :: String -> Maybe (Address arch s)
  -- | Return all of the blocks that contain the given 'Address'
  containingBlocks :: AnalysisResult arch s -> Address arch s -> [Block arch s]
  -- | Pretty print an address
  prettyAddress :: Address arch s -> T.Text
  prettyInstruction :: Address arch s -> Instruction arch s -> T.Text
  prettyOperand :: Address arch s -> Operand arch s -> T.Text
  prettyOpcode :: Opcode arch s -> T.Text

  functions :: AnalysisResult arch s -> [FunctionHandle arch s]
  opcode :: Instruction arch s -> Opcode arch s
  operands :: Instruction arch s -> [Operand arch s]
  boundValue :: Instruction arch s -> Maybe (Operand arch s)
  genericSemantics :: AnalysisResult arch s -> Instruction arch s -> Maybe (ParameterizedFormula arch s)
  functionBlocks :: AnalysisResult arch s -> FunctionHandle arch s -> [Block arch s]

-- | A formula describing the semantics of an instruction
data ParameterizedFormula arch s where
  ParameterizedFormula :: (SA.Architecture arch) => SA.ShapeRepr arch tp -> F.ParameterizedFormula (SB.SimpleBackend s) arch tp -> ParameterizedFormula arch s

prettyParameterizedFormula :: ParameterizedFormula arch s -> T.Text
prettyParameterizedFormula (ParameterizedFormula repr f) = F.printParameterizedFormula repr f

-- | A container for instructions
data Block arch s =
  Block { blockAddress :: !(Address arch s)
        , blockInstructions :: [(Address arch s, Instruction arch s)]
        }
  deriving (Generic)

instance (NFData (Address arch s), NFData (Instruction arch s)) => NFData (Block arch s)

-- | A description of a function suitable for looking up the function definition
-- in the 'ArchResult' for the architecture
data FunctionHandle arch s =
  FunctionHandle { fhAddress :: !(Address arch s)
                 , fhName :: T.Text
                 }
  deriving (Generic)

deriving instance (Show (Address arch s)) => Show (FunctionHandle arch s)
instance (NFData (Address arch s)) => NFData (FunctionHandle arch s)
