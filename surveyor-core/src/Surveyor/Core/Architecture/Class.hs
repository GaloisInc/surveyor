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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Surveyor.Core.Architecture.Class (
  IR(..),
  SomeIRRepr(..),
  Architecture(..),
  SomeResult(..),
  ResultIndex(..),
  AnalysisResult(..),
  BlockMapping(..),
  ParameterizedFormula(..),
  prettyParameterizedFormula,
  Block(..),
  FunctionHandle(..),
  -- * Operand Lists
  OperandList(..),
  OperandListItem(..),
  Delimiter(..),
  fromList,
  fromItemList,
  indexOperandList,
  Zipper,
  zipper,
  zipperNext,
  zipperPrev,
  zipperFocused,
  -- * Constraints
  ArchConstraints
  ) where

import           GHC.Generics ( Generic )

import           Control.Applicative ( (<|>) )
import           Control.DeepSeq ( NFData, rnf, deepseq )
import qualified Control.Once as O
import qualified Data.ByteString as BS
import qualified Data.Map as M
import           Data.Maybe ( isJust )
import           Data.Parameterized.Classes ( testEquality )
import qualified Data.Parameterized.Nonce as NG
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Traversable as TR

import qualified SemMC.Architecture as SA
import qualified SemMC.Formula as F
import qualified Lang.Crucible.Backend.Simple as SB

import           Surveyor.Core.IRRepr ( IRRepr )

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
                               NFData (Operand arch s),
                               NFData (Instruction arch s))


class (IR arch s) => Architecture (arch :: *) (s :: *) where
  data ArchResult arch s :: *

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
  -- the alternative view, the empty list will be returned.  If the requested IR
  -- is not supported, an empty list will also be returned.
  --
  -- FIXME: Some source IR blocks may translate to *multiple* alternative IR
  -- blocks (esp. Crucible)
  asAlternativeIR :: IRRepr arch ir -> AnalysisResult arch s -> FunctionHandle arch s -> IO (Maybe (BlockMapping arch ir s))

data BlockMapping arch ir s =
  BlockMapping { blockMapping :: M.Map (Address arch s) (Block arch s, Block ir s)
               , baseToIRAddrs :: M.Map (Address arch s) (S.Set (Address ir s))
               , irToBaseAddrs :: M.Map (Address ir s) (S.Set (Address arch s))
               }

data OperandList e = OperandList (Seq.Seq (OperandListItem e))
  deriving (Functor, Foldable, Traversable)

instance (NFData e) => NFData (OperandList e) where
  rnf (OperandList s) = s `deepseq` ()

data Delimiter = Parens | Brackets | Braces | Angles

instance NFData Delimiter where
  rnf !_d = ()

data OperandListItem e where
  Item :: e -> OperandListItem e
  Delimited :: Delimiter -> OperandList e -> OperandListItem e

deriving instance Functor OperandListItem
deriving instance Foldable OperandListItem
deriving instance Traversable OperandListItem

instance (NFData e) => NFData (OperandListItem e) where
  rnf i =
    case i of
      Item e -> e `deepseq` ()
      Delimited !_d l -> l `deepseq` ()

fromList :: [Operand arch s] -> OperandList (Operand arch s)
fromList = OperandList . Seq.fromList . map Item

fromItemList :: [OperandListItem e] -> OperandList e
fromItemList = OperandList . Seq.fromList

indexOperandList :: OperandList e -> OperandList (Int, e)
indexOperandList = snd . TR.mapAccumL tagElt 0
  where
    tagElt i elt = (i + 1, (i, elt))

data Zipper e = Zipper (OperandList e) Int e [(Int, OperandList e)]

instance (NFData e) => NFData (Zipper e) where
  rnf (Zipper ol i e ctx) =
    ol `deepseq`
    i `deepseq`
    e `deepseq`
    ctx `deepseq` ()

zipper :: OperandList e -> Maybe (Zipper e)
zipper = go []
  where
    go trail ol@(OperandList s) =
      case Seq.viewl s of
        Seq.EmptyL -> Nothing
        item Seq.:< _ ->
          case item of
            Item i -> Just (Zipper ol 0 i trail)
            Delimited _ s' -> go ((0, OperandList s) : trail) s'

zipperFocused :: Zipper e -> e
zipperFocused (Zipper _ _ e _) = e

zipperNext :: Zipper e -> Maybe (Zipper e)
zipperNext (Zipper ol idx _ trail) = zNextWork ol idx trail

zipperPrev :: Zipper e -> Maybe (Zipper e)
zipperPrev (Zipper ol idx _ trail) = zPrevWork ol idx trail

zPrevWork :: OperandList e -> Int -> [(Int, OperandList e)] -> Maybe (Zipper e)
zPrevWork ol idx trail =
  prevLeftOrUp ol idx trail <|> prevDown trail

prevDown :: [(Int, OperandList e)] -> Maybe (Zipper e)
prevDown trail =
  case trail of
    [] -> Nothing
    (prevIdx, prevList) : rest -> zPrevWork prevList prevIdx rest

prevLeftOrUp :: OperandList e -> Int -> [(Int, OperandList e)] -> Maybe (Zipper e)
prevLeftOrUp ol@(OperandList s) idx trail = do
  let prevIdx = idx - 1
  itm <- s Seq.!? prevIdx
  case itm of
    Item e -> return (Zipper ol prevIdx e trail)
    Delimited _ ol'@(OperandList s') -> prevLeftOrUp ol' (Seq.length s' - 1) ((prevIdx, ol) : trail)

zNextWork :: OperandList e -> Int -> [(Int, OperandList e)] -> Maybe (Zipper e)
zNextWork ol idx trail =
  nextRightOrDown ol idx trail <|> nextUp trail

nextRightOrDown :: OperandList e -> Int -> [(Int, OperandList e)] -> Maybe (Zipper e)
nextRightOrDown ol@(OperandList s) idx trail = do
  let nextIdx = idx + 1
  itm <- s Seq.!? nextIdx
  -- If this is an actual item, advance.  If it is another nested list, descend
  -- and grab the first element.
  case itm of
    Item e -> return (Zipper ol nextIdx e trail)
    Delimited _ ol' -> nextRightOrDown ol' 0 ((nextIdx, ol) : trail)

nextUp :: [(Int, OperandList e)] -> Maybe (Zipper e)
nextUp trail =
  case trail of
    [] -> Nothing
    (prevIdx, prevList) : rest -> zNextWork prevList prevIdx rest

-- | An abstraction over intermediate representations for display in a UI
--
-- This class represents everything we can display in the UI.  This constraint
-- is weaker than 'Architecture', as we can't implement 'Architecture' for some
-- of our IRs (like macaw and crucible).  The split lets us render those
-- architectures without requiring partial instances of 'Architecture'
class (ArchConstraints arch s) => IR (arch :: *) (s :: *) where
  data Instruction arch s :: *
  data Operand arch s :: *
  data Opcode arch s :: *
  data Address arch s :: *
  opcode :: Instruction arch s -> Opcode arch s
  operands :: Instruction arch s -> OperandList (Operand arch s)
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
