{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
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
  Dielectric (..),
  DiffuseLight (..),
  Isotropic (..),
) where

import Control.Applicative (Alternative (..))
import Control.Lens ((^.))
import Control.Monad (guard, join)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.State.Strict (State)
import Data.Coerce (coerce)
import Data.Generics.Labels ()
import Data.Image.Types
import Data.Maybe (fromMaybe)
import Data.Ord (clamp)
import GHC.Generics (Generic, Generic1)
import Graphics.ColorModel
import Linear
import Linear.Affine (Point)
import Linear.Direction
import RayTracing.Object.Shape (HitRecord (..))
import RayTracing.Ray
import RayTracing.Texture
import System.Random (RandomGen)
import System.Random.Stateful (StateGenM (..), applyRandomGenM, randomRM)
import System.Random.Utils (randomPointOnUnitHemisphere, randomPointOnUnitSphere)

newtype Attenuation cs a = Attenuation {getAttenuation :: Color cs a}
  deriving (Generic)
  deriving newtype (Num, Fractional, Unbox)

deriving newtype instance
  (cs ~ RGB, e ~ Double) => Texture (Attenuation cs e)

deriving instance (Show (Color cs a)) => Show (Attenuation cs a)

deriving instance (Eq (Color cs a)) => Eq (Attenuation cs a)

deriving instance (Ord (Color cs a)) => Ord (Attenuation cs a)

deriving instance (Functor (Color cs)) => Functor (Attenuation cs)

deriving instance (Foldable (Color cs)) => Foldable (Attenuation cs)

deriving instance (Traversable (Color cs)) => Traversable (Attenuation cs)

deriving newtype instance (Applicative (Color cs)) => Applicative (Attenuation cs)

deriving newtype instance (Additive (Color cs)) => Additive (Attenuation cs)

pattern MkAttn :: a -> a -> a -> Attenuation RGB a
pattern MkAttn {redRatio, greenRatio, blueRatio} =
  Attenuation (ColorRGB redRatio greenRatio blueRatio)

newtype instance U.Vector (Attenuation cs a) = V_Attenuation (U.Vector (Pixel cs a))

newtype instance U.MVector s (Attenuation cs a) = MV_Attenuation (U.MVector s (Pixel cs a))

infixl 7 .*

(.*) :: (ColorModel cs a) => Attenuation cs a -> Pixel cs a -> Pixel cs a
{-# INLINE (.*) #-}
(.*) = (*) . coerce

data SomeMaterial where
  MkSomeMaterial :: (Material a) => a -> SomeMaterial

instance Material SomeMaterial where
  scatter = \case (MkSomeMaterial mat) -> scatter mat
  {-# INLINE scatter #-}
  emitted = \case (MkSomeMaterial mat) -> emitted mat
  {-# INLINE emitted #-}

class Material a where
  scatter :: (RandomGen g) => a -> HitRecord -> Ray -> MaybeT (State g) (Attenuation RGB Double, Ray)
  emitted :: a -> Point V2 Double -> Point V3 Double -> Color RGB Double
  emitted = const $ const $ const 0
  {-# INLINE emitted #-}

newtype Lambertian txt = Lambertian {albedo :: txt}
  deriving (Show, Eq, Ord, Generic)

instance (Texture txt) => Material (Lambertian txt) where
  {-# INLINE scatter #-}
  scatter Lambertian {..} Hit {..} _ = do
    d <- applyRandomGenM randomPointOnUnitSphere StateGenM
    let sDir = unDir normal ^+^ unDir d
        scattered =
          Ray
            { rayOrigin = coord
            , rayDirection =
                if nearZero sDir
                  then unDir normal
                  else sDir
            }
    pure (Attenuation $ colorAt albedo textureCoordinate coord, scattered)

newtype Hemispheric txt = Hemispheric {albedo :: txt}
  deriving (Show, Eq, Ord, Generic)

instance (Texture txt) => Material (Hemispheric txt) where
  {-# INLINE scatter #-}
  scatter Hemispheric {..} Hit {..} _ = do
    d <- applyRandomGenM (randomPointOnUnitHemisphere normal) StateGenM
    let sDir = unDir normal ^+^ unDir d
        scattered =
          Ray
            { rayOrigin = coord
            , rayDirection =
                if nearZero sDir
                  then unDir normal
                  else sDir
            }
    pure (Attenuation $ colorAt albedo textureCoordinate coord, scattered)

newtype Metal txt = Metal {albedo :: txt}
  deriving (Show, Eq, Ord, Generic, Generic1, Functor)

instance (Texture txt) => Material (Metal txt) where
  scatter Metal {..} Hit {..} inRay = do
    let refled = reflectAround normal $ inRay ^. #rayDirection
        scatterred = Ray {rayOrigin = coord, rayDirection = refled}
        col = colorAt albedo textureCoordinate coord
    guard $ refled `dot` unDir normal > 0
    pure (Attenuation col, scatterred)
  {-# INLINE scatter #-}

data FuzzyMetal txt = FuzzyMetal {albedo :: !txt, fuzz :: !Double}
  deriving (Show, Eq, Ord, Generic, Generic1, Functor)

instance (Texture txt) => Material (FuzzyMetal txt) where
  scatter FuzzyMetal {..} Hit {..} inRay = do
    f <- lift $ (clamp (0.0, 1.0) fuzz *^) <$> applyRandomGenM randomPointOnUnitSphere StateGenM
    let refled = reflectAround normal $ inRay ^. #rayDirection
        scatterred = Ray {rayOrigin = coord, rayDirection = refled ^+^ unDir f}
        col = colorAt albedo textureCoordinate coord
    guard (refled `dot` unDir normal > 0)
    pure (Attenuation col, scatterred)
  {-# INLINE scatter #-}

refract ::
  HitRecord ->
  Dielectric ->
  -- | Surface normal
  Dir V3 Double ->
  -- | Inray
  Dir V3 Double ->
  -- | Reflectance threshold
  Double ->
  -- | Refracted vctor and the refraction
  Maybe (V3 Double)
refract Hit {..} di@Dielectric {..} n r thresh = do
  let q
        | frontFace = recip refractiveIndex
        | otherwise = refractiveIndex
      cosθ = -unDir r `dot` unDir n
      sinθ = sqrt $ 1 - cosθ * cosθ
      rPerp' = q *^ (r |+^ cosθ *| n)
      rPara' = -sqrt (1 - quadrance rPerp') *| n
      refrac = rPerp' ^+^ rPara'
      refl = reflectance di cosθ
  guard $ q * sinθ <= 1.0 && refl <= thresh
  pure refrac

reflectance ::
  Dielectric ->
  -- | Cosine
  Double ->
  Double
reflectance (Dielectric idx) cosine =
  let !r0 = join (*) $ (1 - idx) / (1 + idx)
   in r0 + (1 - r0) * (1 - cosine) ^ (5 :: Int)

newtype Dielectric = Dielectric {refractiveIndex :: Double}
  deriving (Show, Eq, Ord, Generic)

instance Material Dielectric where
  scatter di h@Hit {..} r = do
    thresh <- randomRM (0.0, 1.0) StateGenM
    let scat =
          fromMaybe (reflectAround normal $ r ^. #rayDirection) $
            refract h di normal (dir $ r ^. #rayDirection) thresh
        scattered = Ray {rayOrigin = coord, rayDirection = scat}
    pure (MkAttn 1.0 1.0 1.0, scattered)

newtype DiffuseLight txt = DiffuseLight {emit :: txt}
  deriving (Show, Eq, Ord, Generic, Generic1, Functor, Foldable, Traversable)

instance (Texture txt) => Material (DiffuseLight txt) where
  scatter = const $ const $ const empty
  {-# INLINE scatter #-}
  emitted DiffuseLight {..} = colorAt emit
  {-# INLINE emitted #-}

newtype Isotropic txt = Isotropic {albedo :: txt}
  deriving (Show, Eq, Ord, Generic, Generic1, Functor, Foldable, Traversable)

instance (Texture txt) => Material (Isotropic txt) where
  scatter Isotropic {..} h r = do
    d <- applyRandomGenM randomPointOnUnitSphere StateGenM
    pure (Attenuation $ colorAt albedo (h ^. #textureCoordinate) (h ^. #coord), Ray {rayOrigin = r ^. #rayOrigin, rayDirection = unDir d})
