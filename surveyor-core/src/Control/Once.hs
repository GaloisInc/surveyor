-- | This type is a wrapper around values to ensure that they only need to be
-- fully traversed once to evaluate them to normal form.  Subsequent deepseq
-- calls have no effect.
module Control.Once (
  Once,
  runOnce,
  once
  ) where

import Control.DeepSeq

data Once a = Once () a

runOnce :: Once a -> a
runOnce (Once _ a) = a

-- | Construct the wrapper
--
-- The constructor is set up so that forcing evaluation evaluates the @()@
-- value, which in turn fully evaluates the payload due to the 'rnf' call.
once :: (NFData a) => a -> Once a
once a = Once (rnf a) a

instance NFData (Once a) where
  rnf (Once () _) = ()

instance Foldable Once where
  foldMap f (Once _ a) = f a
