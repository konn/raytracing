{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE LambdaCase #-}

module RayTracing.Object.StdShape (ToStdShape (..), StdShape (..)) where

import Data.Kind (Constraint)
import GHC.Exts (TYPE)
import GHC.Generics (Generic)
import RayTracing.Object.Shape
import RayTracing.Object.Sphere (Sphere)

data StdShape
  = ASphere {-# UNPACK #-} !Sphere
  | Rect {-# UNPACK #-} !Plane
  | Compound ![StdShape]
  deriving (Show, Eq, Ord, Generic)

type ToStdShape :: TYPE rep -> Constraint
class ToStdShape a where
  toStdShape :: a -> StdShape

instance ToStdShape a => ToStdShape [a] where
  toStdShape = Compound . map toStdShape
  {-# INLINE toStdShape #-}
  {-# SPECIALIZE instance ToStdShape [StdShape] #-}
  {-# SPECIALIZE instance ToStdShape [Sphere] #-}
  {-# SPECIALIZE instance ToStdShape [Plane] #-}

instance ToStdShape StdShape where
  {-# SPECIALIZE instance ToStdShape StdShape #-}
  toStdShape = id
  {-# INLINE toStdShape #-}

instance ToStdShape Sphere where
  {-# SPECIALIZE instance ToStdShape Sphere #-}
  toStdShape = ASphere
  {-# INLINE toStdShape #-}

instance ToStdShape Plane where
  {-# SPECIALIZE instance ToStdShape Plane #-}
  toStdShape = Rect
  {-# INLINE toStdShape #-}

instance Hittable StdShape where
  hitWithin = \case
    ASphere sph -> hitWithin sph
    Compound ss -> hitWithin ss
    Rect p -> hitWithin p
  {-# INLINE hitWithin #-}
  boundingBox = \case
    ASphere sph -> boundingBox sph
    Compound ss -> boundingBox ss
    Rect p -> boundingBox p
  {-# INLINE boundingBox #-}
