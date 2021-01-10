{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Surveyor.Core.HandlerMonad (
  HandlerT,
  runHandlerT,
  withFailAction,
  expectValue,
  expectValueWith
  ) where

import qualified Control.Monad.Except as ME
import qualified Control.Monad.Fail as MF
import qualified Control.Monad.Reader as MR
import qualified Control.Monad.Trans as MT
import           Control.Monad.IO.Class ( MonadIO )

data EarlyReturn = ReturnDefault

-- | This monad transformer for supporting early returns inside of event handlers
newtype HandlerT r m a = HandlerT (ME.ExceptT EarlyReturn (MR.ReaderT (m ()) m) a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , ME.MonadError EarlyReturn
           , MR.MonadReader (m ())
           )

-- | The 'MF.MonadFail' instance for this monad initiates an early return,
-- rather than throwing an IO exception.  This is meant to ease pattern matching.
instance (Monad m) => MF.MonadFail (HandlerT r m) where
  fail _ = do
    patternMatchFailAction <- MR.ask
    lift patternMatchFailAction
    ME.throwError ReturnDefault

lift :: (Monad m) => m () -> HandlerT r m ()
lift = HandlerT . MT.lift . MT.lift

-- | Run the handler action, returning the default value if there is an early
-- return
--
-- Handlers always need to return an updated state, thus the type @r@ is fixed.
runHandlerT :: (Monad m) => r -> HandlerT r m r -> m r
runHandlerT def (HandlerT ex) = MR.runReaderT doException (return ())
  where
    doException = do
      res <- ME.runExceptT ex
      case res of
        Left ReturnDefault -> return def
        Right r -> return r

-- | Set a (scoped) action to call if 'MF.fail' is invoked (e.g., via an
-- irrefutable pattern match failure)
--
-- This is very useful because pattern matching is very ergonomic in the
-- presence of GADT constraint recovery
withFailAction :: (Monad m) => m () -> HandlerT r m a -> HandlerT r m a
withFailAction fa = MR.local (\_ -> fa)

-- | If the given value is 'Nothing', return early
--
-- If returning early, run the provided action (e.g., to produce a descriptive
-- error effect)
expectValueWith :: (Monad m) => Maybe a -> m () -> HandlerT r m a
expectValueWith mv onFail =
  case mv of
    Nothing -> lift onFail >> ME.throwError ReturnDefault
    Just v -> return v

expectValue :: (Monad m) => Maybe a -> HandlerT r m a
expectValue mv =
  case mv of
    Nothing -> ME.throwError ReturnDefault
    Just v -> return v
