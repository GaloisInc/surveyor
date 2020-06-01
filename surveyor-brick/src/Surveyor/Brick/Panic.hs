{-# LANGUAGE TemplateHaskell #-}
module Surveyor.Brick.Panic (
  Component(..),
  panic
  ) where

import qualified Panic as P

data Component = Brick
  deriving (Show)

instance P.PanicComponent Component where
  panicComponentName _ = "surveyor-brick"
  panicComponentIssues _ = "https://github.com/GaloisInc/surveyor/issues"
  panicComponentRevision = $(P.useGitRevision)

panic :: (P.HasCallStack) => String -> [String] -> b
panic = P.panic Brick
