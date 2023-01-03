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
  | ABox {-# UNPACK #-} !Box
  | Rotated {-# UNPACK #-} !(Rotate StdShape)
  | Translated {-# UNPACK #-} !(Translate StdShape)
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

instance ToStdShape sh => ToStdShape (Translate sh) where
  {-# SPECIALIZE instance ToStdShape (Translate StdShape) #-}
  {-# SPECIALIZE instance ToStdShape (Translate Sphere) #-}
  {-# SPECIALIZE instance ToStdShape (Translate Plane) #-}
  toStdShape = Translated . fmap toStdShape
  {-# INLINE toStdShape #-}

instance ToStdShape sh => ToStdShape (Rotate sh) where
  {-# SPECIALIZE instance ToStdShape (Rotate StdShape) #-}
  {-# SPECIALIZE instance ToStdShape (Rotate Sphere) #-}
  {-# SPECIALIZE instance ToStdShape (Rotate Plane) #-}
  toStdShape = Rotated . fmap toStdShape
  {-# INLINE toStdShape #-}

instance ToStdShape Box where
  {-# SPECIALIZE instance ToStdShape Box #-}
  toStdShape = ABox
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
    ABox p -> hitWithin p
    Translated p -> hitWithin p
    Rotated p -> hitWithin p
  {-# INLINE hitWithin #-}
  boundingBox = \case
    ASphere sph -> boundingBox sph
    Compound ss -> boundingBox ss
    Rect p -> boundingBox p
    ABox p -> boundingBox p
    Translated p -> boundingBox p
    Rotated p -> boundingBox p
  {-# INLINE boundingBox #-}
