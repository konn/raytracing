{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE RecordWildCards #-}

module RayTracing.Object.Shape.Box (
  Box (..),
) where

import Control.Lens ((^.))
import Control.Monad.Zip (mzip)
import Data.Generics.Labels ()
import GHC.Generics (Generic)
import Linear
import Linear.Affine
import RayTracing.BoundingBox
import RayTracing.Object.Shape.Class
import RayTracing.Object.Shape.Plane

data Box = Box {start, end :: {-# UNPACK #-} !(Point V3 Double)}
  deriving (Show, Eq, Ord, Generic)

instance Hittable Box where
  hitWithin Box {..} =
    hitWithin
      [ xyPlane (start ^. _z) $ mzip (start ^. _xy) (end ^. _xy)
      , xyPlane (end ^. _z) $ mzip (start ^. _xy) (end ^. _xy)
      , yzPlane (start ^. _x) $ mzip (start ^. _yz) (end ^. _yz)
      , yzPlane (end ^. _x) $ mzip (start ^. _yz) (end ^. _yz)
      , zxPlane (start ^. _y) $ mzip (start ^. _zx) (end ^. _zx)
      , zxPlane (end ^. _y) $ mzip (start ^. _zx) (end ^. _zx)
      ]
  {-# INLINE hitWithin #-}
  boundingBox = fmap pure . MkBoundingBox <$> start <*> end
  {-# INLINE boundingBox #-}