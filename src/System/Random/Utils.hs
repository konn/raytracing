module System.Random.Utils (randomPointInUnitSphere) where

import Linear
import Linear.Direction (Dir, unsafeDir)
import System.Random
import System.Random.Stateful

{- |
Instead of sample-and-reject suggested in the tutorial,
we use the spherical coordinate instead; i.e.
sampling θ ∈ [0, π], φ ∈ [-π, π].
-}
randomPointInUnitSphere :: RandomGen g => g -> (Dir V3 Double, g)
randomPointInUnitSphere g = runSTGen g $ \mg -> do
  theta <- (pi *) <$> randomRM (0, 1.0) mg
  phi <- (pi *) <$> randomRM (-0.99999, 1.0) mg
  pure $ unsafeDir $ V3 (sin theta * cos phi) (sin theta * sin phi) (cos theta)
