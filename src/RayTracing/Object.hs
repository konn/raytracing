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
  AScene,
  rayColour,
) where

import Control.Lens ((%~), (^.))
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable (Bitraversable (..), bifoldMapDefault, bimapDefault)
import Data.FMList (FMList)
import Data.Generics.Labels ()
import Data.Image.Types
import GHC.Generics (Generic, Generic1)
import Linear (lerp, normalize, _y)
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
  hits = hits . shape
  {-# INLINE hits #-}
  doesHit = doesHit . shape
  {-# INLINE doesHit #-}
  doesHitWithin = doesHitWithin . shape
  {-# INLINE doesHitWithin #-}

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

newtype SceneOf sh mat = Scene
  { objects :: FMList (Object sh mat)
  }
  deriving (Show, Generic, Generic1, Functor, Foldable, Traversable)

rayColour ::
  ( Hittable sh
  , Material mat
  , RandomGenM g r m
  ) =>
  SceneOf sh mat ->
  g ->
  Int ->
  Ray ->
  m (Pixel Double)
rayColour Scene {..} g = go
  where
    go !depth r@Ray {..}
      | depth <= 0 = pure 0.0
      | Just (hit, obj) <-
          withNearestHitWithin objects (Just 0.001) Nothing r = do
          scatter obj hit r g >>= \case
            Nothing -> pure 0.0
            Just (attenuation, scattered) ->
              (attenuation .*) <$> go (depth - 1) scattered
      | otherwise = do
          let u = normalize rayDirection
              t = 0.5 * (u ^. _y + 1.0)
          pure $ lerp t (Pixel 0.5 0.7 1.0) (Pixel 1.0 1.0 1.0)

type AScene = SceneOf SomeHittable SomeMaterial

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
