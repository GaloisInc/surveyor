{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module Surveyor.Core.OperandList (
  OperandList,
  Delimiter(..),
  OperandListItem(..),
  fromList,
  fromItemList,
  listItems,
  indexOperandList,
  Zipper,
  zipper,
  zipperFocused,
  zipperNext,
  zipperPrev
  ) where

import           Control.Applicative ( (<|>) )
import           Control.DeepSeq
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import qualified Data.Traversable as TR

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

fromList :: [e] -> OperandList (e)
fromList = OperandList . Seq.fromList . map Item

fromItemList :: [OperandListItem e] -> OperandList e
fromItemList = OperandList . Seq.fromList

listItems :: OperandList e -> [OperandListItem e]
listItems (OperandList s) = F.toList s

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
