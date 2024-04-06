{-# LANGUAGE LambdaCase #-}

module RayTracing.Object.Shape.StdShape (
  ToStdShape (..),
  StdShape (..),
  module RayTracing.Object.Shape.Plane,
  module RayTracing.Object.Shape.Sphere,
  module RayTracing.Object.Shape.Rotate,
  module RayTracing.Object.Shape.Translate,
  module RayTracing.Object.Shape.ConstantMedium,
) where

import Data.Kind (Constraint)
import GHC.Exts (TYPE)
import GHC.Generics (Generic)
import RayTracing.Object.Shape.Box (Box)
import RayTracing.Object.Shape.Class
import RayTracing.Object.Shape.ConstantMedium
import RayTracing.Object.Shape.Plane
import RayTracing.Object.Shape.Rotate
import RayTracing.Object.Shape.Sphere
import RayTracing.Object.Shape.Translate

data StdShape
  = ASphere {-# UNPACK #-} !Sphere
  | Rect {-# UNPACK #-} !Plane
  | ABox {-# UNPACK #-} !Box
  | Rotated {-# UNPACK #-} !(Rotate StdShape)
  | Translated {-# UNPACK #-} !(Translate StdShape)
  | Constant {-# UNPACK #-} !(ConstantMedium StdShape)
  | Compound ![StdShape]
  deriving (Show, Eq, Ord, Generic)

type ToStdShape :: TYPE rep -> Constraint
class ToStdShape a where
  toStdShape :: a -> StdShape

instance (ToStdShape a) => ToStdShape [a] where
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

instance (ToStdShape sh) => ToStdShape (Translate sh) where
  {-# SPECIALIZE instance ToStdShape (Translate StdShape) #-}
  {-# SPECIALIZE instance ToStdShape (Translate Sphere) #-}
  {-# SPECIALIZE instance ToStdShape (Translate Plane) #-}
  toStdShape = Translated . fmap toStdShape
  {-# INLINE toStdShape #-}

instance (ToStdShape sh) => ToStdShape (Rotate sh) where
  {-# SPECIALIZE instance ToStdShape (Rotate StdShape) #-}
  {-# SPECIALIZE instance ToStdShape (Rotate Sphere) #-}
  {-# SPECIALIZE instance ToStdShape (Rotate Plane) #-}
  toStdShape = Rotated . fmap toStdShape
  {-# INLINE toStdShape #-}

instance (ToStdShape sh) => ToStdShape (ConstantMedium sh) where
  {-# SPECIALIZE instance ToStdShape (ConstantMedium StdShape) #-}
  {-# SPECIALIZE instance ToStdShape (ConstantMedium Sphere) #-}
  {-# SPECIALIZE instance ToStdShape (ConstantMedium Plane) #-}
  toStdShape = Constant . fmap toStdShape
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
    Constant p -> hitWithin p
  {-# INLINE hitWithin #-}
  boundingBox = \case
    ASphere sph -> boundingBox sph
    Compound ss -> boundingBox ss
    Rect p -> boundingBox p
    ABox p -> boundingBox p
    Translated p -> boundingBox p
    Rotated p -> boundingBox p
    Constant p -> boundingBox p
  {-# INLINE boundingBox #-}
