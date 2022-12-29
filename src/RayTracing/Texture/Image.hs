{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module RayTracing.Texture.Image (ImageTexture, ImageTexture' (..), loadImageTexture) where

import Control.Lens ((%~))
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Image.Types (Pixel (..), RGB)
import Data.Image.Types qualified as AAA
import Data.Massiv.Array qualified as M
import Data.Massiv.Array.IO (Image, liftPixel, readImageAuto, unColorRGB)
import Data.Massiv.Array.IO qualified as C
import Data.Ord (clamp)
import GHC.Generics (Generic)
import Linear
import Linear.Affine (Point (..))
import RIO (MonadIO, fromMaybe)
import RayTracing.Texture

newtype ImageTexture' r = ImageTexture {image :: Image r RGB Double}
  deriving (Generic)

type ImageTexture = ImageTexture' M.S

loadImageTexture :: MonadIO m => FilePath -> m ImageTexture
loadImageTexture =
  fmap (ImageTexture . M.computeP . M.map (liftPixel unColorRGB))
    . readImageAuto @M.S @(C.SRGB 'C.Linear)

instance M.Manifest r (Pixel RGB Double) => Texture (ImageTexture' r) where
  {-# INLINE colorAt #-}
  colorAt ImageTexture {..} p =
    let P (V2 u v) = p <&> clamp (0, 1) & _y %~ (1 -)
        M.Sz2 h w = M.size image
        i = floor (u * fromIntegral (w - 1))
        j = floor (v * fromIntegral (h - 1))
     in const $ coerce $ image M.! (j M.:. i)
