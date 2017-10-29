{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Compose where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.Monad

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a =
  Compose (f (g a))

-- Implement a Functor instance for Compose
instance (Functor f, Functor g) =>
    Functor (Compose f g) where
  (<$>) :: forall a b. (a -> b) -> Compose f g a -> Compose f g b
  f <$> Compose x = Compose helper1 where
    helper1 :: f (g b)
    helper1 = helper2 <$> x

    helper2 :: g a -> g b
    helper2 y = f <$> y

instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure x = Compose (pure (pure x))

  (<*>) :: forall a b.
    Compose f g (a -> b)
    -> Compose f g a
    -> Compose f g b
  Compose f <*> Compose x = Compose helper1 where
    helper1 :: f (g b)
    helper1 = pure helper2 <*> f <*> x

    helper2 :: g (a -> b) -> g a -> g b
    helper2 = (<*>)

instance (Monad f, Monad g) =>
  Monad (Compose f g) where
  (=<<) :: forall a b.
    (a -> Compose f g b)
    -> Compose f g a
    -> Compose f g b
  -- impossible
  (=<<) = undefined
