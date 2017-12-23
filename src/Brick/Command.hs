{-# LANGUAGE PolyKinds #-}
module Brick.Command (
  Command(..)
  ) where

import qualified Data.Functor.Const as C
import qualified Data.Parameterized.List as PL
import qualified Data.Text as T

data Command a r tps =
  Command { cmdName :: T.Text
          -- ^ The name of the command
          , cmdArgNames :: PL.List (C.Const T.Text) tps
          -- ^ Argument names
          , cmdArgTypes :: PL.List r tps
          -- ^ Argument types
          , cmdFunc :: PL.List a tps -> IO ()
          -- ^ A function to call on the argument list
          }
