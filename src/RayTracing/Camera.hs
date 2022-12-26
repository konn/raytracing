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
import Linear.Angle
import Linear.Direction
import RayTracing.Ray

data Camera = Camera
  { cameraOrigin :: {-# UNPACK #-} !(Point V3 Double)
  , horizontal, vertical :: {-# UNPACK #-} !(V3 Double)
  , lowerLeftCorner :: {-# UNPACK #-} !(Point V3 Double)
  }
  deriving (Show, Eq, Ord, Generic)

data CameraConfig = CameraConfig
  { aspectRatio :: {-# UNPACK #-} !Double
  , verticalFieldOfView :: {-# UNPACK #-} !(Angle Double)
  , cameraOrigin :: {-# UNPACK #-} !(Point V3 Double)
  , lookingAt :: {-# UNPACK #-} !(Point V3 Double)
  , viewUp :: {-# UNPACK #-} !(Dir V3 Double)
  }
  deriving (Show, Eq, Ord, Generic)

defaultCameraConfig :: CameraConfig
defaultCameraConfig =
  CameraConfig
    { aspectRatio = 16.0 / 9.0
    , verticalFieldOfView = (pi / 2) @@ rad
    , cameraOrigin = 0
    , lookingAt = P (V3 0 0 (-1))
    , viewUp = dir $ V3 0 1 0
    }

mkCamera :: CameraConfig -> Camera
mkCamera CameraConfig {..} =
  let h = tanA $ verticalFieldOfView ^/ 2.0
      viewportHeight = 2 * h
      viewportWidth = aspectRatio * viewportHeight
      w = dir $ cameraOrigin .-. lookingAt
      u = dir $ unDir viewUp `cross` unDir w
      v = unDir w `cross` unDir u
      horizontal = viewportWidth *^ unDir u
      vertical = viewportHeight *^ v
      lowerLeftCorner =
        cameraOrigin
          .-^ horizontal ^/ 2.0
          .-^ vertical ^/ 2.0
          .-^ unDir w
   in Camera {..}

getRay :: Camera -> Point V2 Double -> Ray
getRay Camera {..} (P (V2 s t)) =
  let rayDirection = lowerLeftCorner .+^ s *^ horizontal .+^ t *^ vertical .-. cameraOrigin
   in Ray {rayOrigin = cameraOrigin, ..}
