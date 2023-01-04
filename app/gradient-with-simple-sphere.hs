{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Lens
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.State.Strict (evalState)
import Data.Image.Types
import Data.Massiv.Array (Ix2 (..), Sz (..))
import Data.Massiv.Array.IO (writeImage)
import Linear
import Linear.Affine (Affine (..), Point (..))
import Numeric.Utils
import RIO.FilePath ((</>))
import RayTracing.Object.Shape
import RayTracing.Ray
import System.Random (mkStdGen)

main :: IO ()
main = writeImage ("workspace" </> "blue-to-white-with-simple-sphere.png") anImage

anImage :: WordImage
anImage = generateImage (Sz2 imageHeight imageWidth) $ \(j :. i) ->
  let !u = fromIntegral i / (fromIntegral imageWidth - 1)
      !v = fromIntegral j / (fromIntegral imageHeight - 1)
      !r =
        Ray
          { rayOrigin = orig
          , rayDirection =
              (lowerLeftCorner .+^ u *^ horizontal .+^ v *^ vertical)
                .-. orig
          }
   in colorRay r

colorRay :: RayColor
colorRay r@Ray {..}
  | Just Hit {..} <- flip evalState (mkStdGen 42) $ runMaybeT $ hitWithin aSphere 0.0001 Infinity r =
      let n = normalize $ rayAt hitTime r .-. center aSphere
       in 0.5 *^ PixelRGB (n ^. _x + 1) (n ^. _y + 1) (n ^. _z + 1)
  | otherwise =
      let !unitDirection = normalize rayDirection
          !t = 0.5 * (unitDirection ^. _y + 1.0)
       in lerp t (PixelRGB 0.5 0.7 1.0) (PixelRGB 1.0 1.0 1.0)

aSphere :: Sphere
aSphere = Sphere {center = P $ V3 0 0 (-1), radius = 0.5}

aspectRatio :: Double
aspectRatio = 16.0 / 9.0

imageWidth, imageHeight :: Int
imageWidth = 400
imageHeight = floor $ fromIntegral imageWidth / aspectRatio

viewportHeight, viewportWidth :: Double
viewportHeight = 2.0
viewportWidth = aspectRatio * viewportHeight

focalLength :: Double
focalLength = 1.0

orig :: Point V3 Double
orig = P $ V3 0 0 0

horizontal, vertical :: V3 Double
horizontal = V3 viewportWidth 0 0
vertical = V3 0 viewportHeight 0

lowerLeftCorner :: Point V3 Double
lowerLeftCorner =
  orig .-^ horizontal ^/ 2.0 .-^ vertical ^/ 2.0 .-^ V3 0 0 focalLength
