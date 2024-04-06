{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-redundant-constraints #-}

module Data.Image.Types (
  Pixel (..),
  Color (..),
  ColorModel,
  RGB,
  DoubleImage,
  WordImage,
  width,
  height,
  fromDoubleImage,
  generateImage,
  correctGamma,
) where

import Control.Arrow ((>>>))
import Data.Massiv.Array
import Data.Massiv.Array qualified as M
import Data.Massiv.Array.IO (Image)
import Graphics.ColorModel
import Linear (Additive (..))

-- Why we need 'Functor' constraint here?
instance (Applicative (Color cs), Functor (Color cs)) => Additive (Color cs) where
  (^+^) = liftA2 (+)
  {-# INLINE (^+^) #-}
  (^-^) = liftA2 (-)
  {-# INLINE (^-^) #-}
  zero = pure 0
  {-# INLINE zero #-}
  liftU2 = liftA2
  {-# INLINE liftU2 #-}
  liftI2 = liftA2
  {-# INLINE liftI2 #-}

deriving newtype instance (Applicative (Color cs)) => Additive (Pixel cs)

-- | Matrix of pixel, 0.0 to 1.0.
type DoubleImage = Image S RGB Double

-- | Matrix of pixel, 0 to 255
type WordImage = Image S RGB Word8

fromDoubleImage ::
  (Functor (Color cs)) =>
  (M.Source r (Pixel cs Double)) =>
  M.Matrix r (Pixel cs Double) ->
  M.Matrix M.D (Pixel cs Word8)
fromDoubleImage = M.map (fmap $ floor . (255.999 *))

width, height :: (Size r) => Matrix r f -> Int
width = M.size >>> \case (Sz2 _ w) -> w
height = M.size >>> \case (Sz2 h _) -> h

-- | almost same as makeArray, but places origin at the lower-left instead of upper-left.
generateImage :: Sz2 -> (Ix2 -> Pixel RGB Double) -> WordImage
generateImage sz =
  M.computeP
    . fromDoubleImage
    . M.reverse M.Dim2
    . M.makeArray @M.D M.Par sz

correctGamma ::
  (ColorModel cs Double) =>
  (M.Source r (Pixel cs Double)) =>
  M.Matrix r (Pixel cs Double) ->
  M.Matrix M.D (Pixel cs Double)
correctGamma = M.map sqrt
