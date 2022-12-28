{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Lens
import Data.Image.Types
import Data.Massiv.Array (Ix2 (..), Sz (..))
import Data.Massiv.Array.IO (writeImage)
import Linear
import Linear.Affine (Affine (..), Point)
import RIO.FilePath ((</>))
import RayTracing.Ray

main :: IO ()
main = writeImage ("workspace" </> "blue-to-white.png") blueToWhite

blueToWhite :: WordImage
blueToWhite = generateImage (Sz2 imageWidth imageHeight) $ \(j :. i) ->
  let !u = fromIntegral i / fromIntegral imageWidth
      !v = fromIntegral j / fromIntegral imageHeight
      !r =
        Ray
          { rayOrigin = origin
          , rayDirection =
              (lowerLeftCorner .+^ u *^ horizontal .+^ v *^ vertical)
                .-. origin
          }
   in colorRay r

colorRay :: RayColor
colorRay Ray {..} =
  let !unitDirection = normalize rayDirection
      !t = 0.5 * (unitDirection ^. _y + 1.0)
   in lerp t (PixelRGB 0.5 0.7 1.0) (PixelRGB 1.0 1.0 1.0)

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

origin :: Point V3 Double
origin = 0.0

horizontal, vertical :: V3 Double
horizontal = V3 viewportWidth 0 0
vertical = V3 0 viewportHeight 0

lowerLeftCorner :: Point V3 Double
lowerLeftCorner =
  origin .-^ horizontal ^/ 2.0 .-^ vertical ^/ 2.0 .-^ V3 0 0 focalLength
