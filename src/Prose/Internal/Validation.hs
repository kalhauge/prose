{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- This is probably not important
module Prose.Internal.Validation where

import Control.Monad
import Control.Monad.State.Class

-- containers
import Data.Sequence qualified as Seq

import Control.Applicative
import Data.Bifunctor

data Validation e a
  = Success a
  | Failure (Seq.Seq e)
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap fn = \case
    Success a -> Success (fn a)
    Failure err -> Failure err

instance Bifunctor Validation where
  bimap fe fa = \case
    Success a -> Success (fa a)
    Failure err -> Failure (fmap fe err)

instance Applicative (Validation e) where
  pure = Success
  a <*> b = case (a, b) of
    (Success ma, Success mb) -> Success (ma mb)
    (Failure err, Success _mb) -> Failure err
    (Success _ma, Failure err) -> Failure err
    (Failure e1, Failure e2) -> Failure (e1 Seq.>< e2)

instance Alternative (Validation e) where
  empty = Failure mempty
  a <|> b = case a of
    Success ma -> Success ma
    Failure e1 -> case b of
      Success mb -> Success mb
      Failure e2 -> Failure (e1 Seq.>< e2)

instance Monad (Validation e) where
  Success a >>= m = m a
  Failure err >>= _ = Failure err

newtype ValidationT m e a = ValidationT {runValidationT :: m (Validation e a)}

class Monad m => MonadValidate e m | m -> e where
  validate :: Validation e a -> m a

instance Monad m => MonadValidate e (ValidationT m e) where
  validate = ValidationT . return

instance Functor m => Functor (ValidationT m e) where
  fmap fn (ValidationT vmea) = ValidationT (fmap (fmap fn) vmea)

instance (Applicative m) => Applicative (ValidationT m e) where
  pure = ValidationT . pure . pure
  ValidationT mf <*> ValidationT ma =
    ValidationT (liftA2 (<*>) mf ma)

instance Monad m => Alternative (ValidationT m e) where
  empty = validate (Failure mempty)
  ValidationT ma <|> ValidationT mb =
    ValidationT $ do
      ma >>= \case
        Failure _ -> mb
        a -> return a

instance Monad m => Monad (ValidationT m e) where
  ValidationT ma >>= fn =
    ValidationT $
      ma >>= \case
        Success a -> runValidationT (fn a)
        Failure err -> return $ Failure err

instance Monad m => MonadPlus (ValidationT m e) where
  mzero = empty
  mplus m1 m2 = m1 <|> m2

instance MonadState s m => MonadState s (ValidationT m e) where
  state fn = ValidationT (Success <$> state fn)
