module Control.Once (
  Once,
  runOnce,
  once
  ) where

import Control.DeepSeq

data Once a = Once !() !a

runOnce :: Once a -> a
runOnce (Once _ a) = a

once :: (NFData a) => a -> Once a
once a = Once (rnf a) a

instance NFData (Once a) where
  rnf (Once () _) = ()

instance Foldable Once where
  foldMap f (Once _ a) = f a
