{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
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
import Data.Bifunctor qualified as Bi
import Data.Foldable (foldlM)
import Data.Maybe (isJust)
import Data.Strict qualified as St
import Data.Strict.Maybe qualified as StMay
import Data.Strict.Tuple (Pair (..))
import Data.Vector.Unboxed.Deriving (derivingUnbox)
import GHC.Exts (UnliftedType)
import GHC.Generics (Generic)
import Linear (V3 (..), _xy, _yz, _zx)
import Linear.Affine
import Linear.Matrix (M23)
import Linear.V2 (V2 (..))
import RayTracing.Ray (Ray (..))

data BoundingBox = MkBoundingBox {lowerBound, upperBound :: {-# UNPACK #-} !(Point V3 Double)}
  deriving (Show, Eq, Ord, Generic)

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

hitsBox :: Ray -> BoundingBox -> Maybe Double -> Maybe Double -> Bool
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
            . Bi.bimap St.toStrict St.toStrict
        )
    {-# INLINE step #-}
    step (mtmin :!: mtmax) ((o :!: d) :!: (lb :!: ub)) = do
      guard $ d /= 0
      let t0 :.. t1 = mkIntvl ((lb - o) / d) ((ub - o) / d)
          !tmin = StMay.maybe t0 (max t0) mtmin
          !tmax = StMay.maybe t1 (min t1) mtmax
      if tmin < tmax
        then Just $ StMay.Just tmin :!: StMay.Just tmax
        else Nothing

data Interval :: UnliftedType where
  (:..) :: {-# UNPACK #-} !Double -> {-# UNPACK #-} !Double -> Interval

mkIntvl :: Double -> Double -> Interval
mkIntvl a b = if a < b then a :.. b else b :.. a

derivingUnbox
  "BoundingBox"
  [t|BoundingBox -> M23 Double|]
  [|\(MkBoundingBox (P l) (P u)) -> V2 l u|]
  [|\(V2 l u) -> MkBoundingBox (P l) (P u)|]
