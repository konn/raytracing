{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}

module RayTracing.Object.Shape.Plane (
  Plane (),
  mkPlane,
  PlaneConfig (..),
  planeAxes,
  xyPlane,
  yzPlane,
  zxPlane,
) where

import Control.Applicative (liftA2)
import Control.Lens (both, (%~), (^.))
import Control.Monad (guard)
import Control.Monad.Zip (munzip)
import Data.Function ((&))
import Data.Generics.Labels ()
import GHC.Generics (Generic)
import Linear
import Linear.Affine
import Linear.Direction
import RayTracing.BoundingBox
import RayTracing.Object.Shape.Class
import RayTracing.Ray (Ray (..), rayAt)

data PlaneConfig = PlaneConfig
  { planeNormal :: {-# UNPACK #-} !(V3 Double)
  , planeSection :: {-# UNPACK #-} !Double
  , planeBoundary :: {-# UNPACK #-} !(V2 (Double, Double))
  }
  deriving (Show, Eq, Ord, Generic)

data Plane = Plane
  { planeNormal :: {-# UNPACK #-} !(Dir V3 Double)
  , planeSection :: {-# UNPACK #-} !Double
  , planeBoundary :: {-# UNPACK #-} !(V2 (Double, Double))
  , planeCoordDir :: {-# UNPACK #-} !(V2 (V3 Double))
  , planeBoundingBox :: {-# UNPACK #-} !BoundingBox
  }
  deriving (Show, Eq, Ord, Generic)

planeAxes :: Dir V3 Double -> V2 (V3 Double)
{-# INLINE planeAxes #-}
planeAxes = go . unDir
  where
    {-# INLINE go #-}
    go n =
      let !xyAbs = norm (n ^. _xy)
       in if nearZero xyAbs
            then
              if n ^. _z < 0
                then V2 (V3 1 0 0) (V3 0 (-1) 0)
                else V2 (V3 1 0 0) (V3 0 1 0)
            else
              let phi
                    | nearZero xyAbs = 0
                    | otherwise = signum (n ^. _y) * acos (n ^. _x / xyAbs)
                  theta = acos (n ^. _z)
                  rot =
                    axisAngle (V3 0 0 1) phi
                      * axisAngle (V3 0 1 0) theta
                  u = rotate rot $ V3 1 0 0
               in V2 u (n `cross` u)

xyPlane :: Double -> V2 (Double, Double) -> Plane
xyPlane section bounds =
  let (lbs, ubs) = munzip bounds
   in Plane
        { planeNormal = unsafeDir $ V3 0 0 1
        , planeSection = section
        , planeBoundary = bounds
        , planeCoordDir = V2 (V3 1 0 0) (V3 0 1 0)
        , planeBoundingBox =
            MkBoundingBox
              { upperBound = P $ V3 (ubs ^. _x) (ubs ^. _y) (section + 1e-4)
              , lowerBound = P $ V3 (lbs ^. _x) (lbs ^. _y) (section - 1e-4)
              }
        }

yzPlane :: Double -> V2 (Double, Double) -> Plane
yzPlane section bounds =
  let (lbs, ubs) = munzip bounds
   in Plane
        { planeNormal = unsafeDir $ V3 1 0 0
        , planeSection = section
        , planeBoundary = bounds
        , planeCoordDir = V2 (V3 0 1 0) (V3 0 0 1)
        , planeBoundingBox =
            MkBoundingBox
              { upperBound = P $ V3 (section + 1e-4) (ubs ^. _x) (ubs ^. _y)
              , lowerBound = P $ V3 (section - 1e-4) (lbs ^. _x) (lbs ^. _y)
              }
        }

zxPlane :: Double -> V2 (Double, Double) -> Plane
zxPlane section bounds =
  let (lbs, ubs) = munzip bounds
   in Plane
        { planeNormal = unsafeDir $ V3 0 1 0
        , planeSection = section
        , planeBoundary = bounds
        , planeCoordDir = V2 (V3 0 0 1) (V3 1 0 0)
        , planeBoundingBox =
            MkBoundingBox
              { upperBound = P $ V3 (ubs ^. _y) (section + 1e-4) (ubs ^. _x)
              , lowerBound = P $ V3 (lbs ^. _y) (section - 1e-4) (lbs ^. _x)
              }
        }

calcPlaneBoundingBox ::
  Dir V3 Double ->
  Double ->
  V2 (V3 Double) ->
  V2 (Double, Double) ->
  BoundingBox
calcPlaneBoundingBox n off uvDir bd =
  let toPt = fromPlaneCoord n off uvDir
      {-# INLINE toPt #-}
      (!lcd, !ucd) = munzip bd & both %~ toPt . P
      calcBound !l !r
        | nearZero (l - r) =
            let !m = (l + r) / 2
                !eps = 1e-4
             in (m - eps, m + eps)
        | l < r = (l, r)
        | otherwise = (r, l)
      (lb, ub) = munzip $ liftA2 calcBound lcd ucd
   in MkBoundingBox {upperBound = P lb, lowerBound = P ub}

{-
>>> planeAxes (V3 0 0 1)
-}

mkPlane :: PlaneConfig -> Plane
{-# INLINE mkPlane #-}
mkPlane pc
  | nearZero $ pc ^. #planeNormal =
      error $
        "Plane normal vector must be non-zero: "
          <> show (pc ^. #planeNormal)
  | otherwise =
      let !n = dir $ pc ^. #planeNormal
          !d = pc ^. #planeSection / norm (pc ^. #planeNormal)
          !bd = pc ^. #planeBoundary
          !axes = planeAxes n
       in Plane
            { planeNormal = n
            , planeSection = d
            , planeCoordDir = axes
            , planeBoundary = bd
            , planeBoundingBox = calcPlaneBoundingBox n d axes bd
            }

fromPlaneCoord :: Dir V3 Double -> Double -> V2 (V3 Double) -> Point V2 Double -> V3 Double
{-# INLINE fromPlaneCoord #-}
fromPlaneCoord normal d uvDir (P uv) =
  sum (liftA2 (*^) uv uvDir) ^-^ (d *^ unDir normal)

instance Hittable Plane where
  {-# INLINE hitWithin #-}
  hitWithin Plane {..} mtmin mtmax ray@Ray {..} = do
    let !denom = unDir planeNormal `dot` rayDirection
    guard $ denom /= 0
    let !hitTime = (planeSection - dot (unDir planeNormal) (unP rayOrigin)) / denom
    guard $ mtmin < hitTime && hitTime < mtmax
    let !p = rayAt hitTime ray
        !uv0 = planeCoordDir !* unP p
    !uv <-
      fmap P $
        sequenceA $
          (\(l, u) c -> ((c - l) / (u - l)) <$ guard (l < c && c < u))
            <$> planeBoundary
            <*> uv0
    pure $! mkHitWithOutwardNormal rayDirection p planeNormal hitTime uv
  {-# INLINE boundingBox #-}
  boundingBox = Just . planeBoundingBox
