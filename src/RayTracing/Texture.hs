{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}

module RayTracing.Texture (
  Texture (..),
  SomeTexture (..),
  SolidColor (..),
  CheckerTexture (..),
  TextureOffset (..),
) where

import Control.Arrow ((>>>))
import Control.Lens ((%~), (^.))
import Data.Bifunctor (Bifunctor (..))
import Data.Fixed (mod')
import Data.Generics.Labels ()
import Data.Image.Types (Color (..), RGB)
import GHC.Generics (Generic, Generic1)
import Linear (_x, _y, _z)
import Linear.Affine (Point, (.+^))
import Linear.V2 (V2)
import Linear.V3 (V3)

data SomeTexture where
  MkSomeTexture :: Texture txt => txt -> SomeTexture

class Texture texture where
  -- | Maps texture and real coordinate to RGB colour
  colorAt :: texture -> Point V2 Double -> Point V3 Double -> Color RGB Double

newtype SolidColor = SolidColor {textureColor :: Color RGB Double}
  deriving (Show, Eq, Ord, Generic)

instance Texture SolidColor where
  colorAt = const . const . textureColor
  {-# INLINE colorAt #-}

deriving via
  SolidColor
  instance
    (cs ~ RGB, e ~ Double) => Texture (Color cs e)

data CheckerTexture even odd = CheckerTexture
  { evenTexture :: !even
  , oddTexture :: !odd
  , xDiv, yDiv, zDiv :: {-# UNPACK #-} !Double
  }
  deriving (Show, Eq, Ord, Generic, Functor, Foldable, Traversable)

instance Bifunctor CheckerTexture where
  bimap f g = #evenTexture %~ f >>> #oddTexture %~ g
  {-# INLINE bimap #-}
  first = (#evenTexture %~)
  {-# INLINE first #-}
  second = (#oddTexture %~)
  {-# INLINE second #-}

instance (Texture even, Texture odd) => Texture (CheckerTexture even odd) where
  colorAt CheckerTexture {..} txtCd realCd =
    let !sines =
          sin (xDiv * realCd ^. _x)
            * sin (yDiv * realCd ^. _y)
            * sin (zDiv * realCd ^. _z)
     in if sines < 0
          then colorAt evenTexture txtCd realCd
          else colorAt oddTexture txtCd realCd
  {-# INLINE colorAt #-}

data TextureOffset txt = TextureOffset
  { offset :: {-# UNPACK #-} !(V2 Double)
  , texture :: txt
  }
  deriving (Show, Eq, Ord, Generic, Generic1, Functor)

instance Texture txt => Texture (TextureOffset txt) where
  colorAt TextureOffset {..} =
    colorAt texture . fmap (`mod'` 1.0) . (.+^ offset)
  {-# INLINE colorAt #-}
