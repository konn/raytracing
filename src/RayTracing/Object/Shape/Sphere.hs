{-# LANGUAGE RecordWildCards #-}

module RayTracing.Object.Shape.Sphere (Sphere (..)) where

import Control.Lens ((^.))
import Control.Monad (guard)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Foldable (find)
import GHC.Generics
import Linear
import Linear.Affine
import Linear.Direction
import RayTracing.BoundingBox
import RayTracing.Object.Shape.Class
import RayTracing.Ray

data Sphere = Sphere
  { center :: {-# UNPACK #-} !(Point V3 Double)
  , radius :: {-# UNPACK #-} !Double
  }
  deriving (Show, Eq, Ord, Generic)

instance Hittable Sphere where
  hitWithin Sphere {..} tmin tmax r@Ray {..} = MaybeT $ pure $ do
    let !oc = rayOrigin .-. center
        !a = quadrance rayDirection
        !b = oc `dot` rayDirection
        !c = quadrance oc - radius * radius
        !delta = b * b - a * c
    guard $ delta >= 0
    hitTime <-
      find
        (\t -> tmin < t && t < tmax)
        [(-b - sqrt delta) / a, (-b + sqrt delta) / a]
    let p = rayAt hitTime r
        n = unsafeDir $ (p .-. center) ^/ radius
        !theta = acos (-unDir n ^. _y)
        !phi = atan2 (-unDir n ^. _z) (unDir n ^. _x) + pi
        !uv = P $ V2 (phi / (2 * pi)) (theta / pi)
    pure $ mkHitWithOutwardNormal rayDirection p n hitTime uv
  boundingBox Sphere {..} =
    Just
      MkBoundingBox
        { lowerBound = center .-^ pure radius
        , upperBound = center .+^ pure radius
        }
