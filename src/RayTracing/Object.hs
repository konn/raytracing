{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module RayTracing.Object (
  Object (.., MkSomeObject),
  SomeObject,
  SceneOf (..),
  Scene,
  rayColour,
  module RayTracing.Object.Material,
  module RayTracing.Object.Shape,
) where

import Control.Lens ((%~))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable (Bitraversable (..), bifoldMapDefault, bimapDefault)
import Data.Generics.Labels ()
import Data.Image.Types
import GHC.Generics (Generic, Generic1)
import RayTracing.Object.Material
import RayTracing.Object.Shape
import RayTracing.Ray
import System.Random.Stateful (RandomGenM)

data Object shape material = Object
  { shape :: !shape
  , material :: !material
  }
  deriving (Show, Eq, Ord, Generic, Generic1, Functor, Foldable, Traversable)

instance Hittable shape => Hittable (Object shape mat) where
  hitWithin = hitWithin . shape
  {-# INLINE hitWithin #-}
  boundingBox = boundingBox . shape
  {-# INLINE boundingBox #-}

instance Material mat => Material (Object shape mat) where
  scatter = scatter . material
  {-# INLINE scatter #-}

type SomeObject = Object SomeHittable SomeMaterial

pattern MkSomeObject ::
  () =>
  (Hittable obj, Material a) =>
  obj ->
  a ->
  Object SomeHittable SomeMaterial
pattern MkSomeObject shape mat = Object (MkSomeHittable shape) (MkSomeMaterial mat)

data SceneOf sh mat = Scene
  { objects :: ![Object sh mat]
  , background :: !(Ray -> Pixel RGB Double)
  }
  deriving (Generic, Generic1, Functor, Foldable, Traversable)

rayColour ::
  ( Hittable sh
  , Material mat
  , RandomGenM g r m
  ) =>
  -- | Threshould to reagard as zero
  Double ->
  SceneOf sh mat ->
  g ->
  Int ->
  Ray ->
  m (Pixel RGB Double)
rayColour eps Scene {..} g = go
  where
    go !depth r
      | depth <= 0 = pure 0.0
      | Just (hit, obj) <-
          withNearestHitWithin (Just eps) Nothing r objects = do
          runMaybeT (scatter obj hit r g) >>= \case
            Nothing -> pure 0.0
            Just (attenuation, scattered) ->
              (attenuation .*) <$> go (depth - 1) scattered
      | otherwise = pure $ background r

type Scene = SceneOf SomeHittable SomeMaterial

instance Bifunctor Object where
  bimap = bimapDefault
  {-# INLINE bimap #-}
  first = (#shape %~)
  {-# INLINE first #-}
  second = (#material %~)
  {-# INLINE second #-}

instance Bifoldable Object where
  bifoldMap = bifoldMapDefault
  {-# INLINE bifoldMap #-}

instance Bitraversable Object where
  bitraverse f g (Object sh m) = Object <$> f sh <*> g m
  {-# INLINE bitraverse #-}
