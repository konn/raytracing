{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fplugin Effectful.Plugin #-}

module RayTracing.Object.Shape.Class (
  Hittable (..),
  HitRecord (..),
  makeNormalOppositeTo,
  mkHitWithOutwardNormal,
  FoldHittables (..),
  SomeHittable (..),
  withNearestHitWithin,
) where

import Control.Applicative (empty)
import Control.Arrow ((&&&), (>>>))
import Control.Lens (view, (%~), (.~))
import Control.Monad (join, (<$!>), (<=<))
import Data.FMList (FMList)
import Data.Generics.Labels ()
import Data.List (foldl')
import Data.Strict.Maybe qualified as StM
import Effectful
import Effectful.NonDet (NonDet, OnEmptyPolicy (..), runNonDet)
import Effectful.State.Static.Local (State)
import GHC.Generics (Generic, Generic1)
import Linear
import Linear.Affine
import Linear.Direction
import RIO (foldMapM)
import RayTracing.BoundingBox
import RayTracing.Ray (Ray (..))
import System.Random (RandomGen)

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
        | otherwise = negateD outNormal
   in Hit {..}

class Hittable obj where
  hitWithin :: RandomGen g => obj -> Double -> Double -> Ray -> Eff '[NonDet, State g] HitRecord
  boundingBox :: obj -> Maybe BoundingBox

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
{-# INLINE toRec #-}
toRec object record =
  let nearestSoFar = hitTime record
   in NearestRecord {..}

withNearestHitWithin ::
  (Foldable t, Hittable obj, RandomGen g) =>
  Double ->
  Double ->
  Ray ->
  t obj ->
  Eff '[NonDet, State g] (HitRecord, obj)
withNearestHitWithin tmin tmax ray =
  StM.maybe empty (pure . (record &&& object))
    <=< foldl'
      ( \sofarM obj -> do
          !sofar <- sofarM
          let tmax' = StM.maybe tmax nearestSoFar sofar
          either
            (const sofar)
            (StM.Just . toRec obj)
            <$!> inject (runNonDet OnEmptyKeep (hitWithin obj tmin tmax' ray))
      )
      (pure StM.Nothing)

instance (Foldable t, Hittable obj) => Hittable (FoldHittables t obj) where
  hitWithin obj tmin tmax ray = fst <$> withNearestHitWithin tmin tmax ray obj
  {-# INLINE hitWithin #-}
  boundingBox (Hittables objs) =
    join $ foldMapM (fmap Just . boundingBox) objs
  {-# INLINE boundingBox #-}

makeNormalOppositeTo :: V3 Double -> HitRecord -> HitRecord
{-# INLINE makeNormalOppositeTo #-}
makeNormalOppositeTo d = do
  n <- view #normal
  let !frontFace = unDir n `dot` d < 0
  #frontFace .~ frontFace >>> if frontFace then id else #normal %~ negateD
