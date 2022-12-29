{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
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
) where

import Control.Lens ((^.))
import Control.Monad (guard, join)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Maybe (MaybeT)
import Data.Coerce (coerce)
import Data.Generics.Labels ()
import Data.Image.Types
import Data.Maybe (fromMaybe)
import Data.Ord (clamp)
import Data.Vector.Unboxed.Deriving (derivingUnbox)
import GHC.Generics (Generic)
import Graphics.ColorModel
import Linear
import Linear.Direction
import RayTracing.Object.Shape (HitRecord (..))
import RayTracing.Ray
import RayTracing.Texture
import System.Random.Orphans ()
import System.Random.Stateful (RandomGenM, applyRandomGenM, randomRM)
import System.Random.Utils (randomPointOnUnitHemisphere, randomPointOnUnitSphere)

newtype Attenuation cs a = Attenuation {getAttenuation :: Color cs a}
  deriving (Generic)
  deriving newtype (Num, Fractional)

deriving newtype instance
  (cs ~ RGB, e ~ Double) => Texture (Attenuation cs e)

deriving instance Show (Color cs a) => Show (Attenuation cs a)

deriving instance Eq (Color cs a) => Eq (Attenuation cs a)

deriving instance Ord (Color cs a) => Ord (Attenuation cs a)

deriving instance Functor (Color cs) => Functor (Attenuation cs)

deriving instance Foldable (Color cs) => Foldable (Attenuation cs)

deriving instance Traversable (Color cs) => Traversable (Attenuation cs)

deriving newtype instance Applicative (Color cs) => Applicative (Attenuation cs)

deriving newtype instance Additive (Color cs) => Additive (Attenuation cs)

pattern MkAttn :: a -> a -> a -> Attenuation RGB a
pattern MkAttn {redRatio, greenRatio, blueRatio} =
  Attenuation (ColorRGB redRatio greenRatio blueRatio)

derivingUnbox
  "Attenuation"
  [t|forall cs a. (ColorModel cs a) => Attenuation cs a -> Pixel cs a|]
  [|coerce|]
  [|coerce|]

infixl 7 .*

(.*) :: (ColorModel cs a) => Attenuation cs a -> Pixel cs a -> Pixel cs a
{-# INLINE (.*) #-}
(.*) = (*) . coerce

data SomeMaterial where
  MkSomeMaterial :: Material a => a -> SomeMaterial

instance Material SomeMaterial where
  scatter = \case (MkSomeMaterial mat) -> scatter mat
  {-# INLINE scatter #-}

class Material a where
  scatter :: RandomGenM g r m => a -> HitRecord -> Ray -> g -> MaybeT m (Attenuation RGB Double, Ray)

newtype Lambertian txt = Lambertian {albedo :: txt}
  deriving (Show, Eq, Ord, Generic)

instance Texture txt => Material (Lambertian txt) where
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
    pure (Attenuation $ colorAt albedo textureCoordinate coord, scattered)

newtype Hemispheric txt = Hemispheric {albedo :: txt}
  deriving (Show, Eq, Ord, Generic)

instance Texture txt => Material (Hemispheric txt) where
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
    pure (Attenuation $ colorAt albedo textureCoordinate coord, scattered)

newtype Metal = Metal {albedo :: Attenuation RGB Double}
  deriving (Show, Eq, Ord, Generic)

instance Material Metal where
  scatter Metal {..} Hit {..} inRay = const $ do
    let refled = reflectAround normal $ inRay ^. #rayDirection
        scatterred = Ray {rayOrigin = coord, rayDirection = refled}
    guard $ refled `dot` unDir normal > 0
    pure (albedo, scatterred)
  {-# INLINE scatter #-}

data FuzzyMetal = FuzzyMetal {albedo :: !(Attenuation RGB Double), fuzz :: !Double}
  deriving (Show, Eq, Ord, Generic)

instance Material FuzzyMetal where
  scatter FuzzyMetal {..} Hit {..} inRay g = do
    f <- lift $ (clamp (0.0, 1.0) fuzz *^) <$> applyRandomGenM randomPointOnUnitSphere g
    let refled = reflectAround normal $ inRay ^. #rayDirection
        scatterred = Ray {rayOrigin = coord, rayDirection = refled ^+^ unDir f}
    guard (refled `dot` unDir normal > 0)
    pure (albedo, scatterred)
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
  scatter di h@Hit {..} r g = do
    thresh <- lift $ randomRM (0.0, 1.0) g
    let scat =
          fromMaybe (reflectAround normal $ r ^. #rayDirection) $
            refract h di normal (dir $ r ^. #rayDirection) thresh
        scattered = Ray {rayOrigin = coord, rayDirection = scat}
    pure (MkAttn 1.0 1.0 1.0, scattered)
