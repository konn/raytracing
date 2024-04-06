{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}

module RayTracing.Texture.Noise.Perlin (
  PerlinNoise (..),
  PerlinMerble (..),
  PerlinSeed (),
  randomPerlinSeed,
  noise,
  turbulate,
) where

import Control.Arrow ((>>>))
import Control.Monad.Trans.State.Strict (State)
import Control.Monad.Zip (munzip)
import Data.Bits
import Data.Functor ((<&>))
import Data.Massiv.Array qualified as M
import Data.Massiv.Array.Unsafe qualified as M
import Data.Strict.Tuple (Pair (..))
import Data.Vector.Shuffle (shuffle)
import Data.Vector.Unboxed qualified as U
import GHC.Float.RealFracMethods (floorDoubleInt, int2Double)
import GHC.Generics (Generic)
import Linear (Additive (..), Metric (..), (*^), (^+^))
import Linear.Affine
import Linear.Direction (Dir, dir, unDir)
import Linear.V3
import RayTracing.Texture
import System.Random.Stateful

data PerlinSeed = PerlinSeed
  { pointSeeds :: {-# UNPACK #-} !(U.Vector (Dir V3 Double))
  , perms :: {-# UNPACK #-} !(V3 (U.Vector Int))
  }
  deriving (Show, Eq, Ord, Generic)

data PerlinNoise = PerlinNoise
  { perlinSeed :: {-# UNPACK #-} !PerlinSeed
  , scale :: {-# UNPACK #-} !Double
  , turbulenceDepth :: {-# UNPACK #-} !Int
  }
  deriving (Show, Eq, Ord, Generic)

data PerlinMerble = PerlinMerble
  { perlinSeed :: {-# UNPACK #-} !PerlinSeed
  , coordPhase :: {-# UNPACK #-} !(V3 Double)
  , turbulenceScale :: {-# UNPACK #-} !Double
  , turbulenceDepth :: {-# UNPACK #-} !Int
  }
  deriving (Show, Eq, Ord, Generic)

pointCount :: Int
{-# INLINE pointCount #-}
pointCount = 256

randomPerlinSeed :: (RandomGen g) => State g PerlinSeed
{-# INLINE randomPerlinSeed #-}
randomPerlinSeed = do
  pointSeeds <-
    U.replicateM pointCount $
      fmap dir $
        sequenceA $
          pure $
            randomRM (-1.0, 1.0) StateGenM
  perms <-
    sequenceA $
      pure $
        applyRandomGenM (shuffle $ U.generate pointCount id) StateGenM
  pure PerlinSeed {..}

noise :: PerlinSeed -> Point V3 Double -> Double
{-# INLINE noise #-}
noise PerlinSeed {..} =
  unP >>> fmap properFractionDoubleInt' >>> munzip >>> \(!ints, !fracs) ->
    let !uvw = fracs * fracs * (3 - 2 *^ fracs)
     in trilinearInterp uvw
          $ M.unsafeBackpermute
            (M.Sz3 2 2 2)
            ( \(M.Ix3 di dj dk) ->
                M.Ix1 $
                  foldl1 xor $
                    liftI2
                      U.unsafeIndex
                      perms
                      (ints ^+^ V3 di dj dk <&> (.&. 255))
            )
          $ M.fromUnboxedVector M.Seq pointSeeds

properFractionDoubleInt' :: Double -> (Int, Double)
properFractionDoubleInt' d =
  let !n = floorDoubleInt d
      !w = d - int2Double n
   in (n, w)

trilinearInterp :: V3 Double -> M.Array M.D M.Ix3 (Dir V3 Double) -> Double
{-# INLINE trilinearInterp #-}
trilinearInterp !uvw =
  M.sum
    . M.imap
      ( \(M.Ix3 (fromIntegral -> !i) (fromIntegral -> !j) (fromIntegral -> !k)) ->
          ( product
              ( liftA2
                  (\u l -> l * u + (1 - l) * (1 - u))
                  uvw
                  (V3 i j k)
              )
              *
          )
            . dot (uvw ^-^ V3 i j k)
            . unDir
      )

turbulate :: Int -> PerlinSeed -> Point V3 Double -> Double
{-# INLINE turbulate #-}
turbulate n ps =
  abs
    . U.sum
    . U.unfoldrExactN
      n
      ( \(p :!: w) ->
          let !ns = w * noise ps p
           in (ns, (2 *^ p) :!: (w * 0.5))
      )
    . (:!: 1.0)

instance Texture PerlinNoise where
  {-# INLINE colorAt #-}
  colorAt PerlinNoise {..} =
    const $
      pure
        . turbulate turbulenceDepth perlinSeed
        . (scale *^)

instance Texture PerlinMerble where
  {-# INLINE colorAt #-}
  colorAt PerlinMerble {..} =
    const $ \pt ->
      pure $!
        0.5
          * ( 1
                + sin
                  ( (coordPhase `dot` unP pt)
                      + turbulenceScale * turbulate turbulenceDepth perlinSeed pt
                  )
            )
