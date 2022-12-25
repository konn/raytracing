{-# LANGUAGE GHC2021 #-}

-- | Directional (unit) vectors, as provided in diagrams-lib.
module Linear.Direction (Dir, dir, unDir, unsafeDir, invert) where

import GHC.Generics (Generic, Generic1)
import Linear

newtype Dir v a = Dir {_unDir :: v a}
  deriving (Show, Eq, Ord, Generic, Generic1, Functor, Foldable)

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
