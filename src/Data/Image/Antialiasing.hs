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
import System.Random.Stateful

randomSamplingAntialias ::
  (RandomGen g, ColorModel cs Double) =>
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
    ST s (Pixel cs Double)
  ) ->
  M.Matrix M.D (Pixel cs Double)
randomSamplingAntialias g0 samples (M.Sz2 h w) f =
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
                    !v = (fromIntegral (h - j - 1) + dy) / (fromIntegral h - 1)
                Avg 1 <$> f g u v
          )

stencilAntialiasing ::
  (RandomGen g, ColorModel cs Double) =>
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
    ST s (Pixel cs Double)
  ) ->
  M.Matrix M.D (Pixel cs Double)
stencilAntialiasing g0 stencilSize (M.Sz2 h w) f =
  let !w' = stencilSize * w
      !h' = stencilSize * h
   in M.delay $
        M.computeP @M.U $
          M.downsample (M.Stride (stencilSize :. stencilSize)) $
            M.dropWindow $
              M.mapStencil M.Reflect (M.avgStencil $ M.Sz2 stencilSize stencilSize) $
                view _3 $
                  M.generateSplitSeedArray @M.U
                    M.defRowMajorUnbalanced
                    g0
                    (pure . split)
                    M.Par
                    (M.Sz2 h' w')
                    ( const $ \(M.Ix2 j i) ->
                        pure <<< flip runSTGen \g ->
                          let u = fromIntegral i / (fromIntegral w' - 1)
                              v = fromIntegral (h' - j - 1) / (fromIntegral h' - 1)
                           in f g u v
                    )
