{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE RecordWildCards #-}

module RayTracing.Camera (
  mkCamera,
  Camera (..),
  CameraConfig (..),
  defaultCameraConfig,
  getRay,
  ThinLens (..),
) where

import Control.Applicative (liftA2)
import Control.Monad.Trans.State.Strict (State)
import GHC.Generics (Generic)
import Linear
import Linear.Affine
import Linear.Angle
import Linear.Direction
import RayTracing.Ray
import System.Random.Stateful (RandomGen, StateGenM (..), applyRandomGenM)
import System.Random.Utils (randomPointInUnitDisk)

data Camera = Camera
  { cameraOrigin :: {-# UNPACK #-} !(Point V3 Double)
  , xUnit, yUnit :: {-# UNPACK #-} !(Dir V3 Double)
  , horizontal, vertical :: {-# UNPACK #-} !(V3 Double)
  , lowerLeftCorner :: {-# UNPACK #-} !(Point V3 Double)
  , lensRadius :: !(Maybe Double)
  }
  deriving (Show, Eq, Ord, Generic)

data ThinLens = ThinLens {aperture, focusDistance :: !Double}
  deriving (Show, Eq, Ord, Generic)

data CameraConfig = CameraConfig
  { aspectRatio :: {-# UNPACK #-} !Double
  , verticalFieldOfView :: {-# UNPACK #-} !(Angle Double)
  , cameraOrigin :: {-# UNPACK #-} !(Point V3 Double)
  , lookingAt :: {-# UNPACK #-} !(Point V3 Double)
  , viewUp :: {-# UNPACK #-} !(Dir V3 Double)
  , thinLens :: !(Maybe ThinLens)
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
    , thinLens = Nothing
    }

mkCamera :: CameraConfig -> Camera
mkCamera CameraConfig {..} =
  let h = tanA $ verticalFieldOfView ^/ 2.0
      viewportHeight = 2 * h
      viewportWidth = aspectRatio * viewportHeight
      w = dir $ cameraOrigin .-. lookingAt
      u@xUnit = dir $ unDir viewUp `cross` unDir w
      -- v is unit, as w ⊥ u
      v@yUnit = unsafeDir $ unDir w `cross` unDir u
      fdist = maybe 1.0 focusDistance thinLens
      horizontal = fdist * viewportWidth *^ unDir u
      vertical = fdist * viewportHeight *^ unDir v
      lowerLeftCorner =
        cameraOrigin
          .-^ horizontal ^/ 2.0
          .-^ vertical ^/ 2.0
          .-^ (fdist *| w)
      lensRadius = (/ 2) . aperture <$> thinLens
   in Camera {..}

getRay :: RandomGen g => Camera -> Point V2 Double -> State g Ray
getRay Camera {..} (P (V2 s t)) = do
  offset <-
    maybe
      (pure 0)
      ( \r ->
          sum
            . liftA2 (^*) (V2 (unDir xUnit) (unDir yUnit))
            . unP
            . (r *^)
            <$> applyRandomGenM randomPointInUnitDisk StateGenM
      )
      lensRadius
  let rayDirection =
        lowerLeftCorner .+^ s *^ horizontal .+^ t *^ vertical .-. cameraOrigin .-^ offset

  pure Ray {rayOrigin = cameraOrigin .+^ offset, ..}
