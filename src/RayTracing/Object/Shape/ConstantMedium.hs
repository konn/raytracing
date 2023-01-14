{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fplugin=Effectful.Plugin #-}

module RayTracing.Object.Shape.ConstantMedium (
  ConstantMedium (ConstantMedium, boundary, density),
) where

import Control.Lens ((^.))
import Control.Monad (guard)
import Data.Generics.Labels ()
import Effectful
import Effectful.NonDet (NonDet)
import Effectful.State.Static.Local (State)
import GHC.Generics
import Linear (norm)
import Linear.Direction (xDir)
import Numeric.Utils
import RayTracing.Object.Shape.Class
import RayTracing.Ray (Ray, rayAt)
import System.Random.Effectful
import System.Random.Stateful (RandomGen, randomM)

data ConstantMedium a = ConstantMedium' {negInvDensity :: !Double, _boundary :: !a}
  deriving (Show, Eq, Ord, Generic, Generic1, Functor, Foldable, Traversable)

pattern ConstantMedium :: Double -> a -> ConstantMedium a
pattern ConstantMedium {density, boundary} <- ConstantMedium' (negate . recip -> !density) boundary
  where
    ConstantMedium d obj =
      ConstantMedium'
        { negInvDensity = -recip d
        , _boundary = obj
        }

instance Hittable a => Hittable (ConstantMedium a) where
  hitWithin ::
    forall g.
    (RandomGen g) =>
    ConstantMedium a ->
    Double ->
    Double ->
    Ray ->
    Eff '[NonDet, State g] HitRecord
  hitWithin ConstantMedium' {..} tmin tmax r = do
    rec1 <- hitWithin _boundary NegativeInfinity Infinity r
    rec2 <- hitWithin _boundary (rec1 ^. #hitTime + 1e-4) Infinity r
    let t1_ = max tmin $ rec1 ^. #hitTime
        t2 = min tmax $ rec2 ^. #hitTime
    guard $ t1_ < t2
    let !t1 = max 0 t1_
        !rayLen = norm $ r ^. #rayDirection
        !distInsBdry = (t2 - t1) * rayLen
    !hitDist <- (negInvDensity *) . log <$> randomM (StaticLocalStateGenM @g)
    guard $ hitDist <= distInsBdry
    let !hitTime = t1 + hitDist / rayLen
        !pt = rayAt hitTime r
    pure
      Hit
        { textureCoordinate = 0
        , normal = xDir
        , hitTime
        , frontFace = True
        , coord = pt
        }
  boundingBox = boundingBox . boundary
  {-# INLINE boundingBox #-}
