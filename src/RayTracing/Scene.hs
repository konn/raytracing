{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas -fsimpl-tick-factor=1000 #-}

module RayTracing.Scene (
  SceneOf (..),
  Scene,
  rayColour,
  module RayTracing.Object.Shape.StdShape,
  module RayTracing.Object.Material,
  module RayTracing.Object,
  module RayTracing.BVH,
) where

import Control.Monad.ST.Strict (ST)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.State.Strict (State)
import Data.Image.Types (Pixel, RGB)
import Data.Vector.Unboxed qualified as U
import GHC.Generics
import Graphics.ColorModel qualified as C
import Numeric.Utils
import RayTracing.BVH
import RayTracing.Object
import RayTracing.Object.Material
import RayTracing.Object.Shape.StdShape
import RayTracing.Ray (Ray)
import System.Random (RandomGen)
import System.Random.Stateful (StateGenM (..), applyRandomGenM)
import System.Random.Stateful.STUnbox

type Scene = SceneOf StdShape SomeMaterial

data SceneOf sh mat = Scene
  { objects :: !(BVH (Object sh mat))
  , background :: !(Ray -> Pixel RGB Double)
  }
  deriving (Generic)

stuGenToStateGen ::
  (RandomGen g, U.Unbox g) =>
  (forall s. STUGenM g s -> ST s a) ->
  State g a
{-# INLINE stuGenToStateGen #-}
stuGenToStateGen =
  ($ StateGenM) . applyRandomGenM . flip runSTUGen

rayColour ::
  ( Hittable sh
  , Material mat
  , RandomGen g
  , U.Unbox g
  ) =>
  -- | Threshould to reagard as zero
  Double ->
  SceneOf sh mat ->
  Int ->
  Ray ->
  State g (Pixel RGB Double)
{-# INLINE rayColour #-}
rayColour eps Scene {..} = go
  where
    {-# INLINE go #-}
    go !lvl r
      | lvl <= 0 = pure 0.0
      | otherwise =
          stuGenToStateGen (nearestHit eps Infinity r objects) >>= \case
            Just (hit, obj) -> do
              let emission =
                    C.Pixel $
                      emitted obj (textureCoordinate hit) (coord hit)
              runMaybeT (scatter obj hit r) >>= \case
                Nothing -> pure emission
                Just (attenuation, scattered) ->
                  (emission +) . (attenuation .*) <$> go (lvl - 1) scattered
            _ -> pure $ background r
