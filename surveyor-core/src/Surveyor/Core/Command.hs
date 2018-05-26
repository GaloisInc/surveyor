{-# LANGUAGE PolyKinds #-}
module Surveyor.Core.Command (
  Command(..)
  ) where

import qualified Data.Functor.Const as C
import qualified Data.Parameterized.List as PL
import qualified Data.Text as T

import qualified Surveyor.Core.Chan as C

data Command e s a r tps =
  Command { cmdName :: T.Text
          -- ^ The name of the command
          , cmdDocstring :: T.Text
          -- ^ Documentation for the command
          , cmdArgNames :: PL.List (C.Const T.Text) tps
          -- ^ Argument names
          , cmdArgTypes :: PL.List r tps
          -- ^ Argument types
          , cmdFunc :: C.Chan e -> s -> PL.List a tps -> IO ()
          -- ^ A function to call on the argument list
          }
