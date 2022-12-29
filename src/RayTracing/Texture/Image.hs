{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE UndecidableInstances #-}

module RayTracing.Texture.Image (ImageTexture, ImageTexture' (..)) where

import Data.Image.Types (Pixel (..), RGB)
import Data.Massiv.Array qualified as M
import Data.Massiv.Array.IO (Image)
import GHC.Generics (Generic)
import RayTracing.Texture

newtype ImageTexture' r = ImageTexture {runImageTexture' :: Image r RGB Double}
  deriving (Generic)

type ImageTexture = ImageTexture' M.S

instance M.Source r (Pixel RGB Double) => Texture (ImageTexture' r) where
