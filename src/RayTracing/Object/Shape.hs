{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module RayTracing.Object.Shape (
  Hittable (..),
  HitRecord (..),
  mkHitWithOutwardNormal,
  inRange,
  FoldHittables (..),
  SomeHittable (..),
  withNearestHitWithin,
) where

import Control.Arrow ((&&&))
import Control.Monad (join)
import Data.FMList (FMList)
import Data.Kind (Constraint)
import Data.List (foldl')
import Data.Strict qualified as Strict
import Data.Strict.Maybe qualified as StM
import GHC.Exts (TYPE)
import GHC.Generics (Generic, Generic1)
import Linear
import Linear.Affine
import Linear.Direction
import RIO (foldMapM)
import RayTracing.BoundingBox
import RayTracing.Ray (Ray (..))

data HitRecord = Hit
  { coord :: {-# UNPACK #-} !(Point V3 Double)
  , normal :: {-# UNPACK #-} !(Dir V3 Double)
  -- ^ Normal vector pointing __against the ray__
  , hitTime :: {-# UNPACK #-} !Double
  , frontFace :: !Bool
  -- ^ 'True' if the ray is outside the object
  , textureCoordinate :: Point V2 Double
  }
  deriving (Show, Eq, Ord, Generic)

mkHitWithOutwardNormal ::
  -- | ray direction
  V3 Double ->
  Point V3 Double ->
  Dir V3 Double ->
  Double ->
  Point V2 Double ->
  HitRecord
mkHitWithOutwardNormal origDir coord outNormal hitTime textureCoordinate =
  let frontFace = origDir `dot` unDir outNormal < 0
      normal
        | frontFace = outNormal
        | otherwise = invert outNormal
   in Hit {..}

type Hittable :: TYPE rep -> Constraint
class Hittable obj where
  hitWithin :: obj -> Maybe Double -> Maybe Double -> Ray -> Maybe HitRecord
  boundingBox :: obj -> Maybe BoundingBox

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
  hitWithin = \case (MkSomeHittable obj) -> hitWithin obj
  {-# INLINE hitWithin #-}
  boundingBox = \case (MkSomeHittable obj) -> boundingBox obj
  {-# INLINE boundingBox #-}

data NearestRecord obj = NearestRecord
  { object :: !obj
  , record :: {-# UNPACK #-} !HitRecord
  , nearestSoFar :: !Double
  }
  deriving (Show, Eq, Ord, Generic, Functor, Foldable)

toRec :: obj -> HitRecord -> NearestRecord obj
toRec object record =
  let nearestSoFar = hitTime record
   in NearestRecord {..}

withNearestHitWithin ::
  (Foldable t, Hittable obj) =>
  Maybe Double ->
  Maybe Double ->
  Ray ->
  t obj ->
  Maybe (HitRecord, obj)
withNearestHitWithin tmin tmax ray =
  fmap (record &&& object)
    . Strict.toLazy
    . foldl'
      ( \sofar obj ->
          let tmax' = StM.maybe tmax (Just . nearestSoFar) sofar
           in StM.maybe
                sofar
                StM.Just
                $ Strict.toStrict
                  (toRec obj <$> hitWithin obj tmin tmax' ray)
      )
      StM.Nothing

instance (Foldable t, Hittable obj) => Hittable (FoldHittables t obj) where
  hitWithin obj tmin tmax ray = fst <$> withNearestHitWithin tmin tmax ray obj
  {-# INLINE hitWithin #-}
  boundingBox (Hittables objs) =
    join $ foldMapM (fmap Just . boundingBox) objs
  {-# INLINE boundingBox #-}
