{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Surveyor.Architecture.Class (
  Architecture(..),
  SomeResult(..),
  ParameterizedFormula(..),
  prettyParameterizedFormula,
  Block(..),
  FunctionHandle(..)
  ) where

import qualified Data.Parameterized.Nonce as NG
import qualified Data.Text as T

import qualified SemMC.Architecture as SA
import qualified SemMC.Formula as F
import qualified Lang.Crucible.Solver.SimpleBackend as SB

data SomeResult s where
  SomeResult :: (Architecture arch s) => AnalysisResult arch s -> SomeResult s

class Architecture (arch :: *) (s :: *) where
  data AnalysisResult arch s :: *
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
prettyParameterizedFormula (ParameterizedFormula repr f) = F.printFormula repr f

-- | A container for instructions
data Block arch s =
  Block { blockAddress :: !(Address arch s)
        , blockInstructions :: [(Address arch s, Instruction arch s)]
        }

-- | A description of a function suitable for looking up the function definition
-- in the 'AnalysisResult' for the architecture
data FunctionHandle arch s =
  FunctionHandle { fhAddress :: !(Address arch s)
                 , fhName :: T.Text
                 }
