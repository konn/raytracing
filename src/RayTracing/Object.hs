{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}

module RayTracing.Object (
  Object (..),
  mkSomeObject,
  SomeObject,
  SceneOf (..),
  Scene,
  rayColour,
  module RayTracing.Object.Material,
  module RayTracing.Object.Shape,
) where

import Control.Lens ((%~))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.State.Strict (State)
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable (Bitraversable (..), bifoldMapDefault, bimapDefault)
import Data.Generics.Labels ()
import Data.Image.Types
import GHC.Generics (Generic, Generic1)
import Numeric.Utils
import RayTracing.Object.Material
import RayTracing.Object.Shape
import RayTracing.Object.StdShape
import RayTracing.Ray
import System.Random.Stateful (RandomGen)

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
  emitted = emitted . material
  {-# INLINE emitted #-}

type SomeObject = Object StdShape SomeMaterial

mkSomeObject ::
  (ToStdShape obj, Material a) =>
  obj ->
  a ->
  Object StdShape SomeMaterial
mkSomeObject shape mat = Object (toStdShape shape) (MkSomeMaterial mat)

data SceneOf sh mat = Scene
  { objects :: ![Object sh mat]
  , background :: !(Ray -> Pixel RGB Double)
  }
  deriving (Generic, Generic1, Functor, Foldable, Traversable)

rayColour ::
  ( Hittable sh
  , Material mat
  , RandomGen g
  ) =>
  -- | Threshould to reagard as zero
  Double ->
  SceneOf sh mat ->
  Int ->
  Ray ->
  State g (Pixel RGB Double)
rayColour eps Scene {..} = go
  where
    go !depth r
      | depth <= 0 = pure 0.0
      | Just (hit, obj) <-
          withNearestHitWithin eps Infinity r objects = do
          let emission =
                Pixel $
                  emitted obj (textureCoordinate hit) (coord hit)
          runMaybeT (scatter obj hit r) >>= \case
            Nothing -> pure emission
            Just (attenuation, scattered) ->
              (emission +) . (attenuation .*) <$> go (depth - 1) scattered
      | otherwise = pure $ background r

type Scene = SceneOf StdShape SomeMaterial

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
