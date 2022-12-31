{-# LANGUAGE ImpredicativeTypes #-}

module System.Random.Utils (
  randomPointOnUnitSphere,
  randomPointOnUnitHemisphere,
  randomPointInUnitDisk,
) where

import Linear
import Linear.Affine (Point (..))
import Linear.Direction (Dir, unDir, unsafeDir)
import System.Random
import System.Random.Stateful

{- |
Instead of sample-and-reject suggested in the tutorial,
we use the spherical coordinate instead; i.e.
sampling θ ∈ [0, π], φ ∈ [-π, π].
-}
randomPointOnUnitSphere :: RandomGen g => g -> (Dir V3 Double, g)
randomPointOnUnitSphere = flip runStateGen $ \mg -> do
  theta <- (pi *) <$> randomRM (0, 1.0) mg
  phi <- (pi *) <$> randomRM (-1.0, 1.0) mg
  pure $ unsafeDir $ V3 (sin theta * cos phi) (sin theta * sin phi) (cos theta)

{- |
Instead of sample-and-reject suggested in the tutorial,
we use the spherical coordinate instead; i.e.
sampling θ ∈ [0, π/2], φ ∈ [-π, π].
-}
randomPointOnUnitHemisphere :: RandomGen g => Dir V3 Double -> g -> (Dir V3 Double, g)
randomPointOnUnitHemisphere normal g = runStateGen g $ \mg -> do
  pt <- applyRandomGenM randomPointOnUnitSphere mg
  pure $
    if unDir pt `dot` unDir normal > 0
      then pt
      else negate <$> pt

randomPointInUnitDisk :: RandomGen g => g -> (Point V2 Double, g)
randomPointInUnitDisk = flip runStateGen $ \g -> do
  r <- randomRM (0, 1) g
  theta <- (2 * pi *) <$> randomRM (-1, 1) g
  pure $ P $ V2 (r * cos theta) (r * sin theta)
