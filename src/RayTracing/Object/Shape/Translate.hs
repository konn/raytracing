{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module RayTracing.Object.Shape.Translate (
  Translate (Translate, displacement, original),
) where

import Control.Arrow ((>>>))
import Control.Lens ((%~), (+~), (^.))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import GHC.Generics (Generic, Generic1)
import Linear
import Linear.Affine
import RayTracing.BoundingBox
import RayTracing.Object.Shape.Class

data Translate a = Translate'
  { displacement_ :: {-# UNPACK #-} !(V3 Double)
  , original_ :: !a
  , tBBox :: !(Maybe BoundingBox)
  }
  deriving (Show, Eq, Ord, Generic, Generic1, Functor, Foldable, Traversable)

{-# COMPLETE Translate #-}

pattern Translate :: Hittable a => () => V3 Double -> a -> Translate a
pattern Translate {displacement, original} <- Translate' displacement original _
  where
    Translate d t =
      Translate'
        { displacement_ = d
        , original_ = t
        , tBBox = translatedBBox d t
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
