{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Surveyor.Core.Command (
  Command(..),
  SomeCommand(..),
  CommandLike(..)
  ) where

import qualified Data.Functor.Const as C
import qualified Data.Parameterized.List as PL
import qualified Data.Text as T

import qualified Surveyor.Core.Chan as C

-- | This is an existential wrapper like 'Data.Parameterized.Some.Some' (intended to
-- hide the type list describing the arguments of each command).  This
-- specialized version is required because we need to fix the kind signature of
-- the @tps@ type parameter, which isn't possible with
-- 'Data.Parameterized.Some.Some'.
data SomeCommand b where
  SomeCommand :: forall b (tps :: [ArgumentKind b]) . Command b tps -> SomeCommand b

data Command (b :: *) (tps :: [ArgumentKind b]) =
  Command { cmdName :: T.Text
          -- ^ The name of the command
          , cmdDocstring :: T.Text
          -- ^ Documentation for the command
          , cmdArgNames :: PL.List (C.Const T.Text) tps
          -- ^ Argument names
          , cmdArgTypes :: PL.List (ArgumentRepr b) tps
          -- ^ Argument types
          , cmdFunc :: C.Chan (EventType b) -> StateType b -> PL.List (ArgumentType b) tps -> IO ()
          -- ^ A function to call on the argument list
          }

class CommandLike b where
  type EventType b
  type StateType b
  type ArgumentType b :: k -> *
  type ArgumentRepr b :: k -> *
  type ArgumentKind b :: k
