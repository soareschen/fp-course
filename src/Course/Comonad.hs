{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Comonad where

import Course.Core
import Course.ExactlyOne
import Course.Extend

-- | All instances of the `Comonad` type-class must satisfy two laws. These
-- laws are not checked by the compiler. These laws are given as:
--
-- * The law of left identity
--   `∀x. copure <<= x ≅ x`
--
-- * The law of right identity
--   `∀f. copure . (f <<=) == f
class Extend f => Comonad f where
  copure ::
    f a
    -> a

-- | Implement the @Comonad@ instance for @ExactlyOne@.
--
-- >>> copure (ExactlyOne 7)
-- 7
instance Comonad ExactlyOne where
  copure ::
    ExactlyOne a
    -> a
  copure (ExactlyOne x) = x

-- | Witness that all things with (<<=) and copure also have (<$>).
--
-- >>> (+10) <$$> ExactlyOne 7
-- ExactlyOne 17
(<$$>) ::
  forall f a b.
  Comonad f =>
  (a -> b)
  -> f a
  -> f b
f <$$> x = helper1 <<= x where
  helper1 :: f a -> b
  helper1 y = f (copure y)
