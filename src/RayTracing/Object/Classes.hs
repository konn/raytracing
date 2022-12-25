{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module RayTracing.Object.Classes (
  Hittable (..),
  HitRecord (..),
  mkHitWithOutwardNormal,
  inRange,
  FoldHittables (..),
  SomeHittable (..),
) where

import Control.Arrow ((>>>))
import Data.FMList (FMList)
import Data.Maybe (isJust)
import Data.Semigroup (Arg (..), Min (..))
import GHC.Generics (Generic, Generic1)
import Linear
import Linear.Affine
import Linear.Direction
import RayTracing.Ray (Ray (..))

data HitRecord = Hit
  { coord :: {-# UNPACK #-} !(Point V3 Double)
  , normal :: {-# UNPACK #-} !(Dir V3 Double)
  -- ^ Normal vector pointing __against the ray__
  , hitTime :: {-# UNPACK #-} !Double
  , frontFace :: !Bool
  -- ^ 'True' if the ray is outside the object
  }
  deriving (Show, Eq, Ord, Generic)

mkHitWithOutwardNormal ::
  -- | ray direction
  V3 Double ->
  Point V3 Double ->
  Dir V3 Double ->
  Double ->
  HitRecord
mkHitWithOutwardNormal origDir coord outNormal hitTime =
  let frontFace = origDir `dot` unDir outNormal < 0
      normal
        | frontFace = outNormal
        | otherwise = invert outNormal
   in Hit {..}

class Hittable obj where
  hits :: Ray -> obj -> Maybe HitRecord
  hits = hitWithin Nothing Nothing
  doesHit :: Ray -> obj -> Bool
  {-# INLINE doesHit #-}
  doesHit = fmap (not . null) . hits

  hitWithin :: Maybe Double -> Maybe Double -> Ray -> obj -> Maybe HitRecord

  doesHitWithin :: Maybe Double -> Maybe Double -> Ray -> obj -> Bool
  {-# INLINE doesHitWithin #-}
  doesHitWithin = fmap (fmap (fmap isJust)) . hitWithin

inRange :: Ord a => Maybe a -> Maybe a -> a -> Bool
{-# INLINE inRange #-}
inRange mmin mmax a = maybe True (<= a) mmin && maybe True (a <=) mmax

newtype FoldHittables t obj = Hittables {hittables :: t obj}
  deriving (Show, Eq, Ord, Generic, Generic1, Functor, Foldable, Traversable)

deriving via
  FoldHittables [] obj
  instance
    (Hittable obj) => Hittable [obj]

deriving via
  FoldHittables FMList obj
  instance
    (Hittable obj) => Hittable (FMList obj)

data SomeHittable where
  MkSomeHittable :: Hittable obj => obj -> SomeHittable

instance Hittable SomeHittable where
  hits ray = \case (MkSomeHittable obj) -> hits ray obj
  {-# INLINE hits #-}
  doesHit ray = \case (MkSomeHittable obj) -> doesHit ray obj
  {-# INLINE doesHit #-}
  hitWithin tmin tmax ray =
    \case (MkSomeHittable obj) -> hitWithin tmin tmax ray obj
  {-# INLINE hitWithin #-}
  doesHitWithin mmin mmax ray =
    \case (MkSomeHittable obj) -> doesHitWithin mmin mmax ray obj
  {-# INLINE doesHitWithin #-}

instance (Foldable t, Hittable obj) => Hittable (FoldHittables t obj) where
  hits ray =
    fmap (getMin >>> (\(Arg _ b) -> b))
      . foldMap (fmap (Min . (Arg <$> hitTime <*> id)) . hits ray)
  {-# INLINE hits #-}
  doesHit = any . doesHit
  {-# INLINE doesHit #-}
  hitWithin tmin tmax ray =
    fmap (getMin >>> (\(Arg _ b) -> b))
      . foldMap (fmap (Min . (Arg <$> hitTime <*> id)) . hitWithin tmin tmax ray)
  {-# INLINE hitWithin #-}
  doesHitWithin = fmap (fmap any) . doesHitWithin
  {-# INLINE doesHitWithin #-}
