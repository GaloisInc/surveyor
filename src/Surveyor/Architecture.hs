{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- | Abstractions over program representations
--
-- These definitions are designed to allow the Surveyor UI to abstractly handle
-- different architectures, building up abstractions over functions, blocks,
-- instructions, and operands.
module Surveyor.Architecture (
  Architecture(..),
  Block(..),
  FunctionHandle(..),
  ParameterizedFormula(..),
  prettyParameterizedFormula,
  SomeResult(..),
  MC.mkPPC32Result,
  MC.mkPPC64Result,
  MC.mkX86Result,
  LL.mkLLVMResult
  ) where

import           Surveyor.Architecture.Class
import qualified Surveyor.Architecture.LLVM as LL
import qualified Surveyor.Architecture.MC as MC
