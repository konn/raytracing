{-# LANGUAGE GHC2021 #-}

-- | Directional (unit) vectors, as provided in diagrams-lib.
module Linear.Direction (
  Dir,
  dir,
  unDir,
  unsafeDir,
  invert,
  reflectAround,
  (*|),
  (|*),
  (|/),
  (|+^),
  (^+|),
  (|+|),
  (|-^),
  (^-|),
  (|-|),
) where

import Data.Coerce (coerce)
import GHC.Generics (Generic, Generic1)
import Linear

newtype Dir v a = Dir {_unDir :: v a}
  deriving (Show, Eq, Ord, Generic, Generic1, Functor, Foldable)

(*|) :: forall v a. (Functor v, Num a) => a -> Dir v a -> v a
(*|) = coerce $ (*^) @v @a

(|*) :: forall v a. (Functor v, Num a) => Dir v a -> a -> v a
(|*) = coerce $ (^*) @v @a

(|/) :: forall v a. (Functor v, Fractional a) => Dir v a -> a -> v a
(|/) = coerce $ (^/) @v @a

infixl 7 *|, |*, |/

(^+|) :: forall v a. (Additive v, Num a) => v a -> Dir v a -> v a
(^+|) = coerce $ (^+^) @v @a

(|+^) :: forall v a. (Additive v, Num a) => Dir v a -> v a -> v a
(|+^) = coerce $ (^+^) @v @a

(|+|) :: forall v a. (Additive v, Num a) => Dir v a -> Dir v a -> v a
(|+|) = coerce $ (^+^) @v @a

(^-|) :: forall v a. (Additive v, Num a) => v a -> Dir v a -> v a
(^-|) = coerce $ (^-^) @v @a

(|-^) :: forall v a. (Additive v, Num a) => Dir v a -> v a -> v a
(|-^) = coerce $ (^-^) @v @a

(|-|) :: forall v a. (Additive v, Num a) => Dir v a -> Dir v a -> v a
(|-|) = coerce $ (^-^) @v @a

infixl 6 |+^, ^+|, |+|, |-^, ^-|, |-|

unDir :: Dir v a -> v a
{-# INLINE unDir #-}
unDir = _unDir

-- | Converts to unit vector, dividing by 2-norm if non-zero.
dir :: (Metric v, Floating a, Epsilon a) => v a -> Dir v a
{-# INLINE dir #-}
dir = Dir . normalize

-- | Unsafely converting a vector to unit vector, WITHOUT normalising it further.
unsafeDir :: v a -> Dir v a
{-# INLINE unsafeDir #-}
unsafeDir = Dir

invert :: (Functor v, Num a) => Dir v a -> Dir v a
{-# INLINE invert #-}
invert = Dir . fmap negate . unDir

reflectAround :: Num a => Dir V3 a -> V3 a -> V3 a
reflectAround (Dir n) v = v ^-^ 2 * n `dot` v *^ n
