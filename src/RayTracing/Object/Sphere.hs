{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE RecordWildCards #-}

module RayTracing.Object.Sphere (Sphere (..), Hittable (..)) where

import Control.Monad (guard)
import Data.Foldable (find)
import GHC.Generics
import Linear
import Linear.Affine
import Linear.Direction
import RayTracing.BoundingBox
import RayTracing.Object.Shape
import RayTracing.Ray

data Sphere = Sphere
  { center :: {-# UNPACK #-} !(Point V3 Double)
  , radius :: {-# UNPACK #-} !Double
  }
  deriving (Show, Eq, Ord, Generic)

instance Hittable Sphere where
  hitWithin Sphere {..} mtmin mtmax r@Ray {..} = do
    let !oc = rayOrigin .-. center
        !a = quadrance rayDirection
        !b = oc `dot` rayDirection
        !c = quadrance oc - radius * radius
        !delta = b * b - a * c
    guard $ delta >= 0
    hitTime <-
      find
        (inRange mtmin mtmax)
        [(-b - sqrt delta) / a, (-b + sqrt delta) / a]
    let p = rayAt hitTime r
        n = unsafeDir $ (p .-. center) ^/ radius
    pure $ mkHitWithOutwardNormal rayDirection p n hitTime
  boundingBox Sphere {..} =
    Just
      MkBoundingBox
        { lowerBound = center .-^ pure radius
        , upperBound = center .+^ pure radius
        }
