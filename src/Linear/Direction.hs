{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | Directional (unit) vectors, as provided in diagrams-lib.
module Linear.Direction (
  Dir,
  dir3,
  dir,
  unDir,
  unsafeDir,
  negateD,
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
  xDir,
  yDir,
  zDir,
  rotateD,
) where

import Control.Lens (coerced, (%~))
import Data.Coerce (coerce)
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Deriving (derivingUnbox)
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

negateD :: (Functor v, Num a) => Dir v a -> Dir v a
{-# INLINE negateD #-}
negateD = Dir . fmap negate . unDir

rotateD :: (Conjugate a, RealFloat a) => Quaternion a -> Dir V3 a -> Dir V3 a
{-# INLINE rotateD #-}
rotateD = (coerced %~) . rotate

reflectAround :: Num a => Dir V3 a -> V3 a -> V3 a
reflectAround (Dir n) v = v ^-^ 2 * n `dot` v *^ n

derivingUnbox
  "Dir"
  [t|forall v a. (U.Unbox (v a)) => Dir v a -> v a|]
  [|_unDir|]
  [|Dir|]

dir3 :: (Floating a, Epsilon a) => a -> a -> a -> Dir V3 a
{-# INLINE dir3 #-}
dir3 = fmap (fmap dir) . V3

xDir, yDir, zDir :: Floating a => Dir V3 a
{-# INLINE xDir #-}
xDir = Dir $ V3 1 0 0
{-# INLINE yDir #-}
yDir = Dir $ V3 0 1 0
{-# INLINE zDir #-}
zDir = Dir $ V3 0 0 1
