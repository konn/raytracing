{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Data.Image.Antialiasing (randomSamplingAntialias, stencilAntialiasing) where

import Control.Arrow ((<<<))
import Control.Lens
import Control.Monad.ST.Strict (ST)
import Data.Avg
import Data.Image.Types
import Data.Massiv.Array (Ix2 (..), Sz2)
import Data.Massiv.Array qualified as M
import Data.Massiv.Array.Unsafe qualified as UM
import System.Random.Stateful

randomSamplingAntialias ::
  RandomGen g =>
  g ->
  Int ->
  Sz2 ->
  -- | The arguments are:
  --
  --   1. PRNG
  --   2. Width ratio in the unit interval [0, 1]
  --   3. Height ratio in the unit interval [0, 1]
  ( forall s.
    STGenM g s ->
    -- Width ratio [0, 1]
    Double ->
    -- Height ratio [0, 1]
    Double ->
    ST s (Pixel Double)
  ) ->
  M.Matrix M.D (Pixel Double)
randomSamplingAntialias g0 samples (M.Sz2 h w) f =
  M.reverse M.Dim2 $
    M.map getAvg $
      M.foldInner $
        view _3 $
          M.generateSplitSeedArray @M.U
            M.defRowMajorUnbalanced
            g0
            (pure . split)
            M.Par
            (M.Sz3 h w samples)
            ( \_ (M.Ix3 j i _) ->
                pure <<< flip runSTGen \g -> do
                  !dx <- randomRM (-1.0, 1.0) g
                  !dy <- randomRM (-1.0, 1.0) g
                  let !u = (fromIntegral i + dx) / (fromIntegral w - 1)
                      !v = (fromIntegral j + dy) / (fromIntegral h - 1)
                  Avg 1 <$> f g u v
            )

stencilAntialiasing ::
  RandomGen g =>
  g ->
  -- | Stencil size
  Int ->
  M.Sz2 ->
  -- | The arguments are:
  --
  --   1. PRNG
  --   2. Width ratio in the unit interval [0, 1]
  --   3. Height ratio in the unit interval [0, 1]
  ( forall s.
    STGenM g s ->
    -- Width ratio [0, 1]
    Double ->
    -- Height ratio [0, 1]
    Double ->
    ST s (Pixel Double)
  ) ->
  M.Matrix M.D (Pixel Double)
stencilAntialiasing g0 stencilSize (M.Sz2 h w) f =
  M.reverse M.Dim2 $
    M.computeP @M.U $
      M.downsample (M.Stride (stencilSize :. stencilSize)) $
        M.computeP @M.U $
          M.mapStencil M.Reflect (M.avgStencil $ M.Sz2 stencilSize stencilSize) $
            view _3 $
              M.generateSplitSeedArray @M.U
                M.defRowMajorUnbalanced
                g0
                (pure . split)
                M.Par
                (M.Sz2 (stencilSize * h) (stencilSize * w))
                ( \_ (M.Ix2 j i) ->
                    pure <<< flip runSTGen \g ->
                      let u = fromIntegral i / (fromIntegral (stencilSize * w) - 1)
                          v = fromIntegral j / (fromIntegral (stencilSize * h) - 1)
                       in f g u v
                )
