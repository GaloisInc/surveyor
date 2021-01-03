{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
module Surveyor.Core.Architecture.Class (
  IR(..),
  SomeIRRepr(..),
  Architecture(..),
  SymbolicArchitecture(..),
  CrucibleExtension(..),
  CruciblePersonality,
  SomeResult(..),
  ResultIndex(..),
  AnalysisResult(..),
  BlockMapping(..),
  ParameterizedFormula(..),
  prettyParameterizedFormula,
  Block(..),
  FunctionHandle(..),
  NamedTerm(..),
  -- * Constraints
  ArchConstraints
  ) where

import qualified Data.Parameterized.SymbolRepr as PSR
import           GHC.Generics ( Generic )

import           Control.DeepSeq ( NFData, rnf, deepseq )
import qualified Control.Once as O
import qualified Data.ByteString as BS
import           Data.Kind ( Type )
import qualified Data.Map as M
import           Data.Maybe ( isJust )
import           Data.Parameterized.Classes ( testEquality )
import qualified Data.Parameterized.Nonce as NG
import qualified Data.Set as S
import qualified Data.Text as T
import qualified What4.BaseTypes as WT
import qualified What4.Expr.Builder as WEB

import qualified SemMC.Architecture as SA
import qualified SemMC.Formula as F
import qualified Lang.Crucible.Backend as CB
import qualified Lang.Crucible.Backend.Simple as SB
import qualified Lang.Crucible.CFG.Core as CCC
import qualified Lang.Crucible.CFG.Extension as CCE
import qualified Lang.Crucible.FunctionHandle as CFH
import qualified Lang.Crucible.Simulator as CS
import qualified Lang.Crucible.Types as CT

import           Surveyor.Core.IRRepr ( IRRepr, Crucible )
import qualified Surveyor.Core.OperandList as OL
import qualified Surveyor.Core.Architecture.NonceCache as SCAN

data SomeResult s arch where
  SomeResult :: (ArchConstraints arch s, Architecture arch s, CrucibleExtension arch) => AnalysisResult arch s -> SomeResult s arch

instance NFData (SomeResult s arch) where
  rnf (SomeResult ar) = ar `deepseq` ()

-- | The type of the crucible extension for this architecture
--
-- For machine code architectures, it will be the MacawExt.  It will be
-- different for JVM and LLVM
class CrucibleExtension arch where
  type family CrucibleExtensionOperand arch :: Type -> Type
  prettyExtensionStmt :: proxy arch -> CCE.StmtExtension (CrucibleExt arch) (CCC.Reg ctx) tp -> T.Text
  prettyExtensionApp :: proxy arch -> CCE.ExprExtension (CrucibleExt arch) (CCC.Reg ctx) tp -> T.Text
  prettyExtensionOperand :: proxy arch -> CrucibleExtensionOperand arch s -> T.Text
  extensionExprOperands :: SCAN.NonceCache s ctx
                        -> NG.NonceGenerator IO s
                        -> CCE.ExprExtension (CrucibleExt arch) (CCC.Reg ctx) tp
                        -> IO [Operand (Crucible arch) s]
  extensionStmtOperands :: SCAN.NonceCache s ctx
                        -> NG.NonceGenerator IO s
                        -> CCE.StmtExtension (CrucibleExt arch) (CCC.Reg ctx) tp
                        -> IO [Operand (Crucible arch) s]
  extensionOperandSelectable :: proxy arch -> CrucibleExtensionOperand arch s -> Bool


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
                               NFData (Operand arch s),
                               NFData (Instruction arch s))

data NamedTerm s where
  NamedTerm :: NG.Nonce s (tp :: WT.BaseType) -> T.Text -> NamedTerm s

-- | The type of the personality for each simulator
--
-- This is a standalone type family because it needs access to a parameter (sym)
-- that isn't a class method.
type family CruciblePersonality arch sym :: Type

-- | A class holding the operations for symbolically-executing code for a given architecture
class (Architecture arch s) => SymbolicArchitecture arch s where
  -- | A function that takes the simulator state and a value that somehow
  -- represents a string and attempts to turn it into a Haskell string.
  --
  -- Note: If the string is symbolic, this will return 'Nothing'; it only works
  -- for concrete strings
  --
  -- Note: This needs to be in IO because some architectures have complex memory
  -- models that can require IO to manipulate (e.g., llvm)
  loadConcreteString :: (CB.IsSymInterface sym)
                     => proxy (arch, s)
                     -> CS.SimState (CruciblePersonality arch sym) sym (CrucibleExt arch) rtp f a
                     -> CT.TypeRepr tp
                     -> CS.RegValue sym tp
                     -> IO (Maybe T.Text)
  archNonceNames :: ( CB.IsSymInterface sym
                    , tp ~ CT.IntrinsicType nm ctx
                    , sym ~ WEB.ExprBuilder s st fs
                    )
                 => proxy (arch, s)
                 -> T.Text
                 -> PSR.SymbolRepr nm
                 -> CT.CtxRepr ctx
                 -> CS.RegEntry sym tp
                 -> [NamedTerm s]

class (IR arch s, CCE.IsSyntaxExtension (CrucibleExt arch)) => Architecture (arch :: Type) (s :: Type) where
  data ArchResult arch s :: Type
  type CrucibleExt arch :: Type

  -- | Extract the nonce for the analysis result
  archNonce :: AnalysisResult arch s -> NG.Nonce s arch
  -- | Return summary information for the artifact being analyzed; the
  -- information can vary by architecture
  summarizeResult :: AnalysisResult arch s -> [(T.Text, T.Text)]
  -- | Return all of the blocks that contain the given 'Address'
  containingBlocks :: AnalysisResult arch s -> Address arch s -> [Block arch s]

  functions :: AnalysisResult arch s -> [FunctionHandle arch s]
  genericSemantics :: AnalysisResult arch s -> Instruction arch s -> Maybe (ParameterizedFormula arch s)
  functionBlocks :: AnalysisResult arch s -> FunctionHandle arch s -> [Block arch s]

  -- | The list of supported alternative IRs for this architecture
  alternativeIRs :: proxy (arch, s) -> [SomeIRRepr arch s]
  -- | Retrieve an alternative representation of the given function (the target
  -- IR is specified by the 'IRRepr').  If there is an error while constructing
  -- the alternative view or if the alternative view requested is not supported,
  -- 'Nothing' will be returned.
  --
  -- Otherwise, two things are returned:
  --
  -- 1. The alternative view (as a collection of blocks)
  --
  -- 2. A mapping from base block addresses to their corresponding alternative
  -- blocks; this mapping is not guaranteed to be complete.
  asAlternativeIR :: IRRepr arch ir -> AnalysisResult arch s -> FunctionHandle arch s -> IO (Maybe ([Block ir s], BlockMapping arch ir s))
  -- | Get the Crucible CFG (if available) for the given function
  crucibleCFG :: AnalysisResult arch s -> FunctionHandle arch s -> IO (Maybe (CCC.AnyCFG (CrucibleExt arch)))
  -- | Allocate a fresh symbolic value of the given type; this is provided so
  -- that architecture-specific symbolic values can be allocated (e.g., for new
  -- intrinsic types).  This function is expected to return Nothing if it cannot
  -- allocate a value for the given type representative.
  freshSymbolicEntry :: (CB.IsSymInterface sym) => proxy (arch, s) -> sym -> CT.TypeRepr tp -> Maybe (IO (CS.RegValue sym tp))
  -- | Collect and return the architecture-specific information required to run
  -- the symbolic execution engine
  symbolicInitializers :: (CB.IsSymInterface sym)
                       => AnalysisResult arch s
                       -> sym
                       -> IO ( CS.IntrinsicTypes sym
                             , CFH.HandleAllocator
                             , CS.FunctionBindings (CruciblePersonality arch sym) sym (CrucibleExt arch)
                             , CS.ExtensionImpl (CruciblePersonality arch sym) sym (CrucibleExt arch)
                             , CruciblePersonality arch sym
                             )
  fromCrucibleBlock :: Maybe (NG.NonceGenerator IO s
                    -> CCC.CFG (CrucibleExt arch) blocks init ret
                    -> CCC.Block (CrucibleExt arch) blocks ret ctx
                    -> IO (Block (Crucible arch) s))

data BlockMapping arch ir s =
  BlockMapping { blockMapping :: M.Map (Address arch s) (Block arch s, Block ir s)
               , baseToIRAddrs :: M.Map (Address arch s) (S.Set (Address ir s))
               , irToBaseAddrs :: M.Map (Address ir s) (S.Set (Address arch s))
               }

-- | An abstraction over intermediate representations for display in a UI
--
-- This class represents everything we can display in the UI.  This constraint
-- is weaker than 'Architecture', as we can't implement 'Architecture' for some
-- of our IRs (like macaw and crucible).  The split lets us render those
-- architectures without requiring partial instances of 'Architecture'
class (ArchConstraints arch s) => IR (arch :: Type) (s :: Type) where
  data Instruction arch s :: Type
  data Operand arch s :: Type
  data Opcode arch s :: Type
  data Address arch s :: Type
  opcode :: Instruction arch s -> Opcode arch s
  operands :: Instruction arch s -> OL.OperandList (Operand arch s)
  boundValue :: Instruction arch s -> Maybe (Operand arch s)
  -- | Parse a string into an architecture-specific 'Address'
  parseAddress :: String -> Maybe (Address arch s)
  -- | Pretty print an address
  prettyAddress :: Address arch s -> T.Text
  prettyInstruction :: Address arch s -> Instruction arch s -> T.Text
  prettyOperand :: Address arch s -> Operand arch s -> T.Text
  prettyOpcode :: Opcode arch s -> T.Text

  showInstructionAddresses :: proxy (arch, s) -> Bool
  operandSelectable :: Operand arch s -> Bool

  -- | If the IR has a "raw" representation of instructions (as
  -- bytes), this is a function that computes it
  rawRepr :: Maybe (Instruction arch s -> Maybe BS.ByteString)

-- | A formula describing the semantics of an instruction
data ParameterizedFormula arch s where
  ParameterizedFormula :: (SA.Architecture arch) => SA.ShapeRepr arch tp -> F.ParameterizedFormula (SB.SimpleBackend s fs) arch tp -> ParameterizedFormula arch s

prettyParameterizedFormula :: ParameterizedFormula arch s -> T.Text
prettyParameterizedFormula (ParameterizedFormula repr f) = F.printParameterizedFormula repr f

-- | A container for instructions
data Block arch s =
  Block { blockFunction :: !(FunctionHandle arch s)
        , blockAddress :: !(Address arch s)
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

deriving instance (Eq (Address arch s)) => Eq (FunctionHandle arch s)
deriving instance (Ord (Address arch s)) => Ord (FunctionHandle arch s)

deriving instance (Show (Address arch s)) => Show (FunctionHandle arch s)
instance (NFData (Address arch s)) => NFData (FunctionHandle arch s)


-- | A wrapper type around an 'IRRepr' that captures some additional constraints
-- that are needed to use them in some cases
data SomeIRRepr arch s where
  SomeIRRepr :: (ArchConstraints ir s, IR ir s) => IRRepr arch ir -> SomeIRRepr arch s
-- Note: this is here because it depends on ArchConstraints and IR

instance Eq (SomeIRRepr arch s) where
  SomeIRRepr r1 == SomeIRRepr r2 = isJust (testEquality r1 r2)
