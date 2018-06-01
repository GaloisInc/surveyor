-- | A wrapper to enforce that its contents are held in normal form (according to 'NFData')
--
-- This is much like the 'Once' type, except that it holds its @()@ value
-- strictly, which always forces evaluation as long as the NF value itself is
-- evaluated to WHNF.
module Control.NF (
  NF,
  getNF,
  nf
  ) where

import Control.DeepSeq

-- | A wrapper ensuring normal form of values
data NF a = NF !() a

-- | Extract the (already fully evaluated) value from the normal form wrapper
getNF :: NF a -> a
getNF (NF _ a) = a

-- | Construct a normal form wrapper around a value.  If you want to be sure
-- that this works as intended, seq the result of 'nf' to ensure that it is in
-- WHNF, which is sufficient to fully evaluate the contents.
nf :: (NFData a) => a -> NF a
nf a = NF (rnf a) a

instance NFData (NF a) where
  rnf (NF () _) = ()

instance Foldable NF where
  foldMap f (NF _ a) = f a
