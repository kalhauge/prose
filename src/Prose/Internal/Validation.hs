{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- This is probably not important
module Prose.Internal.Validation where

import Data.Bifunctor

data Validation e a
  = Success a
  | Failure e
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap fn = \case
    Success a -> Success (fn a)
    Failure err -> Failure err

instance Bifunctor Validation where
  bimap fe fa = \case
    Success a -> Success (fa a)
    Failure err -> Failure (fe err)

instance Monoid e => Applicative (Validation e) where
  pure = Success
  a <*> b = case (a, b) of
    (Success ma, Success mb) -> Success (ma mb)
    (Failure err, Success _mb) -> Failure err
    (Success _ma, Failure err) -> Failure err
    (Failure e1, Failure e2) -> Failure (e1 <> e2)

instance Monoid e => Monad (Validation e) where
  Success a >>= m = m a
  Failure err >>= _ = Failure err
