{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedLabels #-}

module RayTracing.Object (
  Object (..),
  mkSomeObject,
  SomeObject,
  module RayTracing.Object.Material,
  module RayTracing.Object.Shape,
) where

import Control.Lens ((%~))
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable (Bitraversable (..), bifoldMapDefault, bimapDefault)
import Data.Generics.Labels ()
import GHC.Generics (Generic, Generic1)
import RayTracing.Object.Material
import RayTracing.Object.Shape

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

type SomeObject = Object SomeHittable SomeMaterial

mkSomeObject ::
  (Hittable obj, Material a) =>
  obj ->
  a ->
  Object SomeHittable SomeMaterial
mkSomeObject shape mat = Object (MkSomeHittable shape) (MkSomeMaterial mat)

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
