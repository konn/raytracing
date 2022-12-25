module System.Random.Utils (randomPointInUnitSphere) where

import Linear
import Linear.Affine
import System.Random
import System.Random.Stateful

{- |
Instead of sample-and-reject suggested in the tutorial,
we use the spherical coordinate instead; i.e.
sampling inside r ∈ (0, 1], θ ∈ [0, π], φ ∈ [-π, π].
-}
randomPointInUnitSphere :: RandomGen g => g -> (Point V3 Double, g)
randomPointInUnitSphere g = runSTGen g $ \mg -> do
  r <- randomRM (0, 1.0) mg
  theta <- (pi *) <$> randomRM (0, 1.0) mg
  phi <- (pi *) <$> randomRM (-0.99999, 1.0) mg
  pure $ P $ V3 (r * sin theta * cos phi) (r * sin theta * sin phi) (r * cos theta)
