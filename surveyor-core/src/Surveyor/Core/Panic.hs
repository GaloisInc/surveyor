{-# LANGUAGE TemplateHaskell #-}
module Surveyor.Core.Panic (
  Component(..),
  panic
  ) where

import qualified Panic as P

data Component = Core
  deriving (Show)

instance P.PanicComponent Component where
  panicComponentName _ = "surveyor-core"
  panicComponentIssues _ = "https://github.com/GaloisInc/surveyor/issues"
  panicComponentRevision = $(P.useGitRevision)

panic :: (P.HasCallStack) => String -> [String] -> b
panic = P.panic Core
