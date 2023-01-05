{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module RayTracing.Object.Shape.Rotate (
  Rotate (Rotate, rotation, original),
) where

import Control.Applicative (liftA2)
import Control.Arrow ((>>>))
import Control.Lens (Traversable1 (..), (%~), (^.))
import Control.Monad.Zip (munzip)
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import Data.Semigroup (Max (..), Min (..))
import Data.Semigroup.Foldable (fold1)
import GHC.Generics (Generic, Generic1)
import Linear
import Linear.Affine
import Linear.Direction
import RIO (NonEmpty (..))
import RayTracing.BoundingBox
import RayTracing.Object.Shape.Class

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
  hitWithin Rotate' {..} tmin tmax ray g = do
    let !relativeRay =
          ray
            & #rayOrigin . _Point %~ rotate invRot_
            & #rayDirection %~ rotate invRot_
    hitWithin original_ tmin tmax relativeRay g
      <&> #normal %~ rotateD rotation_
      <&> #coord . _Point %~ rotate rotation_
      <&> makeNormalOppositeTo (ray ^. #rayDirection)
  {-# INLINE boundingBox #-}
  boundingBox = rBBox
