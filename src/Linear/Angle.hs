{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImpredicativeTypes #-}

-- | Opaque angle type and their interface, inspired by diagrams-lib
module Linear.Angle (
  Angle,
  axisAngleA,
  rad,
  deg,
  turn,
  (@@),
  sinA,
  cosA,
  tanA,
  asinA,
  acosA,
  atanA,
) where

import Control.Lens (AReview, Iso', coerced, iso, review, view)
import Data.Coerce (coerce)
import Data.Distributive
import Data.Functor.Rep (Co (..), Representable, collectRep, distributeRep)
import Data.Monoid (Sum (..))
import GHC.Generics (Generic, Generic1)
import Linear
import Linear.Direction

newtype Angle a = Angle a
  deriving (Show, Eq, Ord, Generic, Generic1, Functor, Foldable, Traversable)
  deriving newtype (Enum)
  deriving anyclass (Representable, Additive, Metric)
  deriving (Applicative) via Co Angle
  deriving (Semigroup, Monoid) via Sum a

factored :: (Fractional a) => a -> Iso' (Angle a) a
{-# INLINE factored #-}
{-# SPECIALIZE factored :: Double -> Iso' (Angle Double) Double #-}
factored a = coerced . iso (* a) (/ a)

rad :: Iso' (Angle a) a
{-# SPECIALIZE INLINE rad :: Iso' (Angle Double) Double #-}
{-# INLINE rad #-}
rad = coerced

infixl 5 @@

(@@) :: b -> AReview a b -> a
(@@) = flip review

deg :: (Floating a) => Iso' (Angle a) a
{-# SPECIALIZE INLINE deg :: Iso' (Angle Double) Double #-}
{-# INLINE deg #-}
deg = factored (180 / pi)

turn :: (Floating a) => Iso' (Angle a) a
{-# SPECIALIZE INLINE turn :: Iso' (Angle Double) Double #-}
{-# INLINE turn #-}
turn = factored (1 / (2 * pi))

instance Distributive Angle where
  collect = collectRep
  {-# INLINE collect #-}
  distribute = distributeRep
  {-# INLINE distribute #-}

sinA :: (Floating a) => Angle a -> a
sinA = sin . coerce

cosA :: (Floating a) => Angle a -> a
cosA = cos . coerce

tanA :: (Floating a) => Angle a -> a
tanA = tan . coerce

atanA :: (Floating a) => a -> Angle a
atanA = Angle . atan

acosA :: (Floating a) => a -> Angle a
acosA = Angle . acos

asinA :: (Floating a) => a -> Angle a
asinA = Angle . asin

axisAngleA :: (Floating a, Epsilon a) => Dir V3 a -> Angle a -> Quaternion a
{-# INLINE axisAngleA #-}
axisAngleA d = axisAngle (unDir d) . view rad
