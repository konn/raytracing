{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE RecordWildCards #-}

module RayTracing.Texture.Noise.Perlin (
  PerlinNoise,
  randomPerlinNoise,
  noise,
) where

import Control.Applicative (liftA2)
import Control.Monad.Trans.State.Strict (State)
import Data.Bits
import Data.Vector.Shuffle (shuffle)
import Data.Vector.Unboxed qualified as U
import GHC.Generics (Generic)
import Linear ((*^))
import Linear.Affine
import Linear.V3
import RayTracing.Texture
import System.Random.Stateful

data PerlinNoise = PerlinNoise
  { pointSeeds :: {-# UNPACK #-} !(U.Vector Double)
  , perms :: {-# UNPACK #-} !(V3 (U.Vector Int))
  }
  deriving (Show, Eq, Ord, Generic)

pointCount :: Int
{-# INLINE pointCount #-}
pointCount = 256

randomPerlinNoise :: RandomGen g => State g PerlinNoise
{-# INLINE randomPerlinNoise #-}
randomPerlinNoise = do
  pointSeeds <- U.replicateM pointCount $ randomRM (0.0, 1.0) StateGenM
  perms <-
    sequenceA $
      pure $
        applyRandomGenM (shuffle $ U.generate pointCount id) StateGenM
  pure PerlinNoise {..}

noise :: PerlinNoise -> Point V3 Double -> Double
{-# INLINE noise #-}
noise PerlinNoise {..} =
  U.unsafeIndex pointSeeds . foldl1 xor
    <$> ( liftA2 U.unsafeIndex perms
            . fmap ((.&. 255) . floor)
            . (4 *^)
            . unP
        )

instance Texture PerlinNoise where
  {-# INLINE colorAt #-}
  colorAt pn = const $ pure . noise pn
