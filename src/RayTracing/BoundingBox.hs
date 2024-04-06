{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnliftedDatatypes #-}

module RayTracing.BoundingBox (
  BoundingBox (..),
  volume,
  surfaceArea,
  hitsBox,
) where

import Control.Lens ((^.))
import Control.Monad (guard)
import Data.Array.Accelerate (Elt)
import Data.Array.Accelerate.Linear.Affine ()
import Data.Foldable (foldlM)
import Data.Maybe (isJust)
import Data.Strict qualified as St
import Data.Strict.Tuple (Pair (..))
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as MG
import Data.Vector.Unboxed qualified as U
import GHC.Exts (UnliftedType)
import GHC.Generics (Generic)
import Linear (V3 (..), _xy, _yz, _zx)
import Linear.Affine
import Linear.Matrix (M23)
import RayTracing.Ray (Ray (..))

data BoundingBox = MkBoundingBox {lowerBound, upperBound :: {-# UNPACK #-} !(Point V3 Double)}
  deriving (Show, Eq, Ord, Generic)
  deriving (U.Unbox) via (M23 Double)
  deriving anyclass (Elt)

deriving anyclass instance U.IsoUnbox BoundingBox (M23 Double)

newtype instance U.Vector BoundingBox = V_BoundingBox (U.Vector (M23 Double))

newtype instance U.MVector s BoundingBox = MV_BoundingBox (U.MVector s (M23 Double))

deriving via
  BoundingBox `U.As` M23 Double
  instance
    G.Vector U.Vector BoundingBox

deriving via
  BoundingBox `U.As` M23 Double
  instance
    MG.MVector U.MVector BoundingBox

volume :: BoundingBox -> Double
{-# INLINE volume #-}
volume = product . ((.-.) <$> upperBound <*> lowerBound)

surfaceArea :: BoundingBox -> Double
{-# INLINE surfaceArea #-}
surfaceArea MkBoundingBox {..} =
  let !delta = upperBound .-. lowerBound
      !w = product $ delta ^. _yz
      !h = product $ delta ^. _zx
      !d = product $ delta ^. _xy
      !vol = w * h * d
   in vol

instance Semigroup BoundingBox where
  l <> r =
    MkBoundingBox
      { lowerBound = min <$> lowerBound l <*> lowerBound r
      , upperBound = max <$> upperBound l <*> upperBound r
      }

hitsBox :: Ray -> BoundingBox -> Double -> Double -> Bool
{-# INLINE hitsBox #-}
hitsBox = go
  where
    {-# INLINE go #-}
    go Ray {..} MkBoundingBox {..} =
      St.curry
        ( isJust
            . flip
              (foldlM step)
              ( (:!:)
                  <$> ((:!:) <$> unP rayOrigin <*> rayDirection)
                  <*> ((:!:) <$> unP lowerBound <*> unP upperBound)
              )
        )
    {-# INLINE step #-}
    step (mtmin :!: mtmax) ((o :!: d) :!: (lb :!: ub)) = do
      guard $ d /= 0
      let t0 :.. t1 = mkIntvl ((lb - o) / d) ((ub - o) / d)
          !tmin = max t0 mtmin
          !tmax = min t1 mtmax
      if tmin < tmax
        then Just $ tmin :!: tmax
        else Nothing

data Interval :: UnliftedType where
  (:..) :: {-# UNPACK #-} !Double -> {-# UNPACK #-} !Double -> Interval

mkIntvl :: Double -> Double -> Interval
mkIntvl a b = if a < b then a :.. b else b :.. a
