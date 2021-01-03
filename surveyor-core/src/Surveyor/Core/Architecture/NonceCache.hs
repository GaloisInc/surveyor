{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module Surveyor.Core.Architecture.NonceCache (
  NonceCache(..),
  cacheSize,
  initialCache
  ) where

import           Data.Kind ( Type )
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.Nonce as PN
import qualified Data.Parameterized.TraversableFC as FC
import qualified Lang.Crucible.Types as LCT

data NonceCache s ctx where
  NonceCache :: forall s ctx (k :: LCT.CrucibleType -> Type) . (k ~ PN.Nonce s) => Ctx.Assignment k ctx -> NonceCache s ctx

cacheSize :: NonceCache s ctx -> Ctx.Size ctx
cacheSize (NonceCache a) = Ctx.size a

initialCache :: PN.NonceGenerator IO s -> LCT.CtxRepr ctx -> IO (NonceCache s ctx)
initialCache ng cr = NonceCache <$> FC.traverseFC (\_ -> PN.freshNonce ng) cr
