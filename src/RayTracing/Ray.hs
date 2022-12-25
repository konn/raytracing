{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE RecordWildCards #-}

module RayTracing.Ray (Ray (..), rayAt, RayColor) where

import Data.Image.Types
import GHC.Generics (Generic)
import Linear
import Linear.Affine (Affine (..), Point)

data Ray = Ray
  { rayOrigin :: {-# UNPACK #-} !(Point V3 Double)
  , rayDirection :: {-# UNPACK #-} !(V3 Double)
  }
  deriving (Show, Eq, Ord, Generic)

rayAt :: Double -> Ray -> Point V3 Double
rayAt t Ray {..} = rayOrigin .+^ t *^ rayDirection

type RayColor = Ray -> Pixel Double
