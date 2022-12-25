{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE RecordWildCards #-}

module RayTracing.Camera (
  mkCamera,
  Camera (..),
  CameraConfig (..),
  defaultCameraConfig,
  getRay,
) where

import GHC.Generics (Generic)
import Linear
import Linear.Affine
import RayTracing.Ray

data Camera = Camera
  { cameraOrigin :: {-# UNPACK #-} !(Point V3 Double)
  , horizontal, vertical :: {-# UNPACK #-} !(V3 Double)
  , lowerLeftCorner :: {-# UNPACK #-} !(Point V3 Double)
  }
  deriving (Show, Eq, Ord, Generic)

data CameraConfig = CameraConfig
  { aspectRatio :: {-# UNPACK #-} !Double
  , viewportHeight :: {-# UNPACK #-} !Double
  , focalLength :: {-# UNPACK #-} !Double
  , cameraOrigin :: {-# UNPACK #-} !(Point V3 Double)
  }
  deriving (Show, Eq, Ord, Generic)

defaultCameraConfig :: CameraConfig
defaultCameraConfig =
  CameraConfig
    { aspectRatio = 16.0 / 9.0
    , viewportHeight = 2.0
    , focalLength = 1.0
    , cameraOrigin = 0
    }

mkCamera :: CameraConfig -> Camera
mkCamera CameraConfig {..} =
  let viewportWidth = aspectRatio * viewportHeight
      horizontal = V3 viewportWidth 0 0
      vertical = V3 0 viewportHeight 0
      lowerLeftCorner =
        cameraOrigin
          .-^ horizontal ^/ 2.0
          .-^ vertical ^/ 2.0
          .-^ V3 0 0 focalLength
   in Camera {..}

getRay :: Camera -> Point V2 Double -> Ray
getRay Camera {..} (P (V2 u v)) =
  let rayDirection = lowerLeftCorner .+^ u *^ horizontal .+^ v *^ vertical .-. cameraOrigin
   in Ray {rayOrigin = cameraOrigin, ..}
