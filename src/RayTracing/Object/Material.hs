{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fprint-potential-instances #-}

module RayTracing.Object.Material (
  Attenuation (.., MkAttn, redRatio, greenRatio, blueRatio),
  (.*),
  Material (..),
  Lambertian (..),
  Hemispheric (..),
  Metal (..),
  FuzzyMetal (..),
  SomeMaterial (..),
) where

import Control.Applicative (Applicative (..))
import Control.Lens ((^.))
import Control.Monad (guard)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Maybe (MaybeT)
import Data.Coerce (coerce)
import Data.Generics.Labels ()
import Data.Image.Types
import Data.Ord (clamp)
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Deriving (derivingUnbox)
import GHC.Generics (Generic)
import Linear
import Linear.Direction
import RayTracing.Object.Shape (HitRecord (..))
import RayTracing.Ray
import System.Random.Orphans ()
import System.Random.Stateful (RandomGenM, applyRandomGenM)
import System.Random.Utils (randomPointOnUnitHemisphere, randomPointOnUnitSphere)

newtype Attenuation a = Attenuation {getAttenuation :: Pixel a}
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (Num, Fractional)

pattern MkAttn :: a -> a -> a -> Attenuation a
pattern MkAttn {redRatio, greenRatio, blueRatio} =
  Attenuation (Pixel redRatio greenRatio blueRatio)

derivingUnbox
  "Attenuation"
  [t|forall a. U.Unbox a => Attenuation a -> Pixel a|]
  [|coerce|]
  [|coerce|]

infixl 7 .*

(.*) :: Num a => Attenuation a -> Pixel a -> Pixel a
{-# INLINE (.*) #-}
(.*) = liftA2 (*) . coerce

data SomeMaterial where
  MkSomeMaterial :: Material a => a -> SomeMaterial

instance Material SomeMaterial where
  scatter = \case (MkSomeMaterial mat) -> scatter mat
  {-# INLINE scatter #-}

class Material a where
  scatter :: RandomGenM g r m => a -> HitRecord -> Ray -> g -> MaybeT m (Attenuation Double, Ray)

newtype Lambertian = Lambertian {albedo :: Attenuation Double}
  deriving (Show, Eq, Ord, Generic)

instance Material Lambertian where
  {-# INLINE scatter #-}
  scatter Lambertian {..} Hit {..} _ g = do
    d <- lift $ applyRandomGenM randomPointOnUnitSphere g
    let sDir = unDir normal ^+^ unDir d
        scattered =
          Ray
            { rayOrigin = coord
            , rayDirection =
                if nearZero sDir
                  then unDir normal
                  else sDir
            }
    pure (albedo, scattered)

newtype Hemispheric = Hemispheric {albedo :: Attenuation Double}
  deriving (Show, Eq, Ord, Generic)

instance Material Hemispheric where
  {-# INLINE scatter #-}
  scatter Hemispheric {..} Hit {..} _ g = do
    d <- lift $ applyRandomGenM (randomPointOnUnitHemisphere normal) g
    let sDir = unDir normal ^+^ unDir d
        scattered =
          Ray
            { rayOrigin = coord
            , rayDirection =
                if nearZero sDir
                  then unDir normal
                  else sDir
            }
    pure (albedo, scattered)

newtype Metal = Metal {albedo :: Attenuation Double}
  deriving (Show, Eq, Ord, Generic)

instance Material Metal where
  scatter Metal {..} Hit {..} inRay = const $ do
    let refled = reflectAround normal $ inRay ^. #rayDirection
        scatterred = Ray {rayOrigin = coord, rayDirection = refled}
    guard $ refled `dot` unDir normal > 0
    pure (albedo, scatterred)
  {-# INLINE scatter #-}

data FuzzyMetal = FuzzyMetal {albedo :: Attenuation Double, fuzz :: !Double}
  deriving (Show, Eq, Ord, Generic)

instance Material FuzzyMetal where
  scatter FuzzyMetal {..} Hit {..} inRay g = do
    f <- lift $ (clamp (0.0, 1.0) fuzz *^) <$> applyRandomGenM randomPointOnUnitSphere g
    let refled = reflectAround normal $ inRay ^. #rayDirection
        scatterred = Ray {rayOrigin = coord, rayDirection = refled ^+^ unDir f}
    guard (refled `dot` unDir normal > 0)
    pure (albedo, scatterred)
  {-# INLINE scatter #-}
