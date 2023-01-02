{-# LANGUAGE GHC2021 #-}
{-# HLINT ignore "Use id" #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module RayTracing.Object.StdShape (ToStdShape (..), StdShape (..)) where

import Data.Kind (Constraint)
import GHC.Exts (TYPE)
import GHC.Generics (Generic)
import RayTracing.Object.Shape
import RayTracing.Object.Sphere (Sphere)

data StdShape
  = ASphere {-# UNPACK #-} !Sphere
  | Compound {-# UNPACK #-} ![StdShape]
  deriving (Show, Eq, Ord, Generic)

type ToStdShape :: TYPE rep -> Constraint
class ToStdShape a where
  toStdShape :: a -> StdShape

instance ToStdShape a => ToStdShape [a] where
  toStdShape = Compound . map toStdShape
  {-# INLINE toStdShape #-}
  {-# SPECIALIZE instance ToStdShape [StdShape] #-}
  {-# SPECIALIZE instance ToStdShape [Sphere] #-}

instance ToStdShape StdShape where
  {-# SPECIALIZE instance ToStdShape StdShape #-}
  toStdShape = \a -> a
  {-# INLINE toStdShape #-}

instance ToStdShape Sphere where
  {-# SPECIALIZE instance ToStdShape Sphere #-}
  toStdShape = ASphere
  {-# INLINE toStdShape #-}

instance Hittable StdShape where
  hitWithin = \case
    ASphere sph -> hitWithin sph
    Compound ss -> hitWithin ss
  {-# INLINE hitWithin #-}
  boundingBox = \case
    ASphere sph -> boundingBox sph
    Compound ss -> boundingBox ss
  {-# INLINE boundingBox #-}
