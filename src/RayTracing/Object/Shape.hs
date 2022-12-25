{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module RayTracing.Object.Shape (
  Hittable (..),
  HitRecord (..),
  mkHitWithOutwardNormal,
  inRange,
  withNearestHit,
  FoldHittables (..),
  SomeHittable (..),
  withNearestHitWithin,
) where

import Control.Arrow ((>>>))
import Data.FMList (FMList)
import Data.Maybe (isJust)
import Data.Semigroup (Any (..), Arg (..), Min (..))
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
  hits :: obj -> Ray -> Maybe HitRecord
  hits obj = hitWithin obj Nothing Nothing
  doesHit :: obj -> Ray -> Bool
  {-# INLINE doesHit #-}
  doesHit = fmap (not . null) . hits

  hitWithin :: obj -> Maybe Double -> Maybe Double -> Ray -> Maybe HitRecord

  doesHitWithin :: obj -> Maybe Double -> Maybe Double -> Ray -> Bool
  {-# INLINE doesHitWithin #-}
  doesHitWithin = fmap (fmap (fmap isJust)) . hitWithin

inRange :: Ord a => Maybe a -> Maybe a -> a -> Bool
{-# INLINE inRange #-}
inRange mmin mmax a = maybe True (< a) mmin && maybe True (a <) mmax

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
  hits = \case (MkSomeHittable obj) -> hits obj
  {-# INLINE hits #-}
  doesHit = \case (MkSomeHittable obj) -> doesHit obj
  {-# INLINE doesHit #-}
  hitWithin =
    \case (MkSomeHittable obj) -> hitWithin obj
  {-# INLINE hitWithin #-}
  doesHitWithin =
    \case (MkSomeHittable obj) -> doesHitWithin obj
  {-# INLINE doesHitWithin #-}

withNearestHit ::
  (Foldable t, Hittable obj) =>
  t obj ->
  Ray ->
  Maybe (HitRecord, obj)
withNearestHit =
  fmap (fmap (getMin >>> \(Arg _ b) -> b))
    . foldMap
      ( \obj ->
          fmap (Min . (Arg <$> hitTime <*> (,obj))) . hits obj
      )

withNearestHitWithin ::
  (Foldable t, Hittable obj) =>
  t obj ->
  Maybe Double ->
  Maybe Double ->
  Ray ->
  Maybe (HitRecord, obj)
withNearestHitWithin =
  fmap (fmap $ fmap $ fmap (getMin >>> \(Arg _ b) -> b))
    . foldMap
      ( \obj ->
          fmap (fmap $ fmap $ Min . (Arg <$> hitTime <*> (,obj))) . hitWithin obj
      )

instance (Foldable t, Hittable obj) => Hittable (FoldHittables t obj) where
  hits = fmap (fmap fst) . withNearestHit
  {-# INLINE hits #-}
  doesHit = fmap getAny . foldMap (fmap Any . doesHit)
  {-# INLINE doesHit #-}
  hitWithin = fmap (fmap $ fmap $ fmap fst) . withNearestHitWithin
  {-# INLINE hitWithin #-}
  doesHitWithin = fmap (fmap $ fmap getAny) . foldMap (fmap (fmap $ fmap Any) . doesHitWithin)
  {-# INLINE doesHitWithin #-}
