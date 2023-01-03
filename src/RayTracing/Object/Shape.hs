{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module RayTracing.Object.Shape (
  Hittable (..),
  HitRecord (..),
  mkHitWithOutwardNormal,
  inRange,
  FoldHittables (..),
  SomeHittable (..),
  withNearestHitWithin,
  Plane (),
  mkPlane,
  PlaneConfig (..),
  planeAxes,
  xyPlane,
  yzPlane,
  zxPlane,
  Box (..),
  Rotate (Rotate, rotation, original),
  Translate (Translate, displacement, orig),
) where

import Control.Applicative (liftA2)
import Control.Arrow ((&&&), (>>>))
import Control.Lens (Traversable1 (..), both, view, (%~), (+~), (.~), (^.))
import Control.Monad (guard, join)
import Control.Monad.Zip (munzip, mzip)
import Data.Coerce (coerce)
import Data.FMList (FMList)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import Data.Kind (Constraint)
import Data.List (foldl')
import Data.Semigroup (Max (..), Min (..))
import Data.Semigroup.Foldable (fold1)
import Data.Strict qualified as Strict
import Data.Strict.Maybe qualified as StM
import GHC.Exts (TYPE)
import GHC.Generics (Generic, Generic1)
import Linear
import Linear.Affine
import Linear.Direction
import RIO (NonEmpty (..), foldMapM)
import RayTracing.BoundingBox
import RayTracing.Ray (Ray (..), rayAt)

data HitRecord = Hit
  { coord :: {-# UNPACK #-} !(Point V3 Double)
  , normal :: {-# UNPACK #-} !(Dir V3 Double)
  -- ^ Normal vector pointing __against the ray__
  , hitTime :: {-# UNPACK #-} !Double
  , frontFace :: !Bool
  -- ^ 'True' if the ray is outside the object
  , textureCoordinate :: Point V2 Double
  }
  deriving (Show, Eq, Ord, Generic)

mkHitWithOutwardNormal ::
  -- | ray direction
  V3 Double ->
  Point V3 Double ->
  Dir V3 Double ->
  Double ->
  Point V2 Double ->
  HitRecord
mkHitWithOutwardNormal origDir coord outNormal hitTime textureCoordinate =
  let frontFace = origDir `dot` unDir outNormal < 0
      normal
        | frontFace = outNormal
        | otherwise = negateD outNormal
   in Hit {..}

type Hittable :: TYPE rep -> Constraint
class Hittable obj where
  hitWithin :: obj -> Maybe Double -> Maybe Double -> Ray -> Maybe HitRecord
  boundingBox :: obj -> Maybe BoundingBox

inRange :: Ord a => Maybe a -> Maybe a -> a -> Bool
{-# INLINE inRange #-}
inRange mmin mmax a = maybe True (<= a) mmin && maybe True (a <=) mmax

newtype FoldHittables t obj = Hittables {hittables :: t obj}
  deriving (Show, Eq, Ord, Generic, Generic1, Functor, Foldable, Traversable)

deriving via
  FoldHittables [] obj
  instance
    (Hittable obj) => Hittable [obj]

deriving via
  FoldHittables FMList obj
  instance
    (Hittable obj) => Hittable (FMList obj)

data SomeHittable where
  MkSomeHittable :: Hittable obj => obj -> SomeHittable

instance Hittable SomeHittable where
  hitWithin = \case (MkSomeHittable obj) -> hitWithin obj
  {-# INLINE hitWithin #-}
  boundingBox = \case (MkSomeHittable obj) -> boundingBox obj
  {-# INLINE boundingBox #-}

data NearestRecord obj = NearestRecord
  { object :: !obj
  , record :: {-# UNPACK #-} !HitRecord
  , nearestSoFar :: !Double
  }
  deriving (Show, Eq, Ord, Generic, Functor, Foldable)

toRec :: obj -> HitRecord -> NearestRecord obj
toRec object record =
  let nearestSoFar = hitTime record
   in NearestRecord {..}

withNearestHitWithin ::
  (Foldable t, Hittable obj) =>
  Maybe Double ->
  Maybe Double ->
  Ray ->
  t obj ->
  Maybe (HitRecord, obj)
withNearestHitWithin tmin tmax ray =
  fmap (record &&& object)
    . Strict.toLazy
    . foldl'
      ( \sofar obj ->
          let tmax' = StM.maybe tmax (Just . nearestSoFar) sofar
           in StM.maybe
                sofar
                StM.Just
                $ Strict.toStrict
                  (toRec obj <$> hitWithin obj tmin tmax' ray)
      )
      StM.Nothing

instance (Foldable t, Hittable obj) => Hittable (FoldHittables t obj) where
  hitWithin obj tmin tmax ray = fst <$> withNearestHitWithin tmin tmax ray obj
  {-# INLINE hitWithin #-}
  boundingBox (Hittables objs) =
    join $ foldMapM (fmap Just . boundingBox) objs
  {-# INLINE boundingBox #-}

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
    guard $ maybe True (< hitTime) mtmin && maybe True (hitTime <) mtmax
    let !p = rayAt hitTime ray
        !uv0 = planeCoordDir !* unP p
    !uv <-
      fmap P $
        sequenceA $
          (\(l, u) c -> ((c - l) / (u - l)) <$ guard (l < c && c < u))
            <$> planeBoundary
            <*> uv0
    pure $! mkHitWithOutwardNormal rayDirection p planeNormal hitTime uv
  boundingBox = Just . planeBoundingBox
  {-# INLINE boundingBox #-}

data Translate a = Translate'
  { displacement_ :: {-# UNPACK #-} !(V3 Double)
  , original_ :: !a
  , tBBox :: !(Maybe BoundingBox)
  }
  deriving (Show, Eq, Ord, Generic, Generic1, Functor, Foldable, Traversable)

{-# COMPLETE Translate #-}

pattern Translate :: Hittable a => () => V3 Double -> a -> Translate a
pattern Translate {displacement, orig} <- Translate' displacement orig _
  where
    Translate displacement original =
      Translate'
        { displacement_ = displacement
        , original_ = original
        , tBBox = translatedBBox displacement original
        }

translatedBBox :: Hittable a => V3 Double -> a -> Maybe BoundingBox
translatedBBox disp =
  boundingBox
    >>> fmap
      (#lowerBound . _Point +~ disp >>> #upperBound . _Point +~ disp)

instance Hittable a => Hittable (Translate a) where
  {-# INLINE hitWithin #-}
  hitWithin = \case
    Translate' {..} ->
      \mtmin mtmax ray ->
        hitWithin
          original_
          mtmin
          mtmax
          (ray & #rayOrigin %~ (.-^ displacement_))
          <&> #coord . _Point +~ displacement_
          <&> makeNormalOppositeTo (ray ^. #rayDirection)
  {-# INLINE boundingBox #-}
  boundingBox = tBBox

makeNormalOppositeTo :: V3 Double -> HitRecord -> HitRecord
{-# INLINE makeNormalOppositeTo #-}
makeNormalOppositeTo d = do
  n <- view #normal
  let !frontFace = unDir n `dot` d < 0
  #frontFace .~ frontFace >>> if frontFace then id else #normal %~ negateD

data Rotate a = Rotate'
  { rotation_, invRot_ :: {-# UNPACK #-} !(Quaternion Double)
  , original_ :: a
  , rBBox :: !(Maybe BoundingBox)
  }
  deriving (Show, Eq, Ord, Generic, Generic1, Functor, Foldable, Traversable)

{-# COMPLETE Rotate :: Rotate #-}

pattern Rotate :: Hittable a => () => Quaternion Double -> a -> Rotate a
pattern Rotate {rotation, original} <- Rotate' rotation _ original _
  where
    Rotate q o = Rotate' q (conjugate q) o (rotatedBBox q o)

rotatedBBox :: Hittable a => Quaternion Double -> a -> Maybe BoundingBox
{-# INLINE rotatedBBox #-}
rotatedBBox rot =
  boundingBox >>> fmap \MkBoundingBox {..} ->
    let lrot = lowerBound & _Point %~ rotate rot
        rrot = upperBound & _Point %~ rotate rot
        (mins, maxs) =
          munzip $
            fold1 $
              traverse1 (\(a, b) -> (Min a, Max a) :| [(Min b, Max b)]) $
                liftA2 (,) (unP lrot) (unP rrot)
     in MkBoundingBox
          { upperBound = coerce maxs
          , lowerBound = coerce mins
          }

instance Hittable a => Hittable (Rotate a) where
  {-# INLINE hitWithin #-}
  hitWithin Rotate' {..} tmin tmax ray = do
    let !relativeRay =
          ray
            & #rayOrigin . _Point %~ rotate invRot_
            & #rayDirection %~ rotate invRot_
    hitWithin original_ tmin tmax relativeRay
      <&> #normal %~ rotateD rotation_
      <&> #coord . _Point %~ rotate rotation_
      <&> makeNormalOppositeTo (ray ^. #rayDirection)
  {-# INLINE boundingBox #-}
  boundingBox = rBBox

data Box = Box {start, end :: {-# UNPACK #-} !(Point V3 Double)}
  deriving (Show, Eq, Ord, Generic)

instance Hittable Box where
  hitWithin Box {..} =
    hitWithin
      [ xyPlane (start ^. _z) $ mzip (start ^. _xy) (end ^. _xy)
      , xyPlane (end ^. _z) $ mzip (start ^. _xy) (end ^. _xy)
      , yzPlane (start ^. _x) $ mzip (start ^. _yz) (end ^. _yz)
      , yzPlane (end ^. _x) $ mzip (start ^. _yz) (end ^. _yz)
      , zxPlane (start ^. _y) $ mzip (start ^. _zx) (end ^. _zx)
      , zxPlane (end ^. _y) $ mzip (start ^. _zx) (end ^. _zx)
      ]
  {-# INLINE hitWithin #-}
  boundingBox = fmap pure . MkBoundingBox <$> start <*> end
  {-# INLINE boundingBox #-}