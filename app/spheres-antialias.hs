{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Lens
import Data.Avg
import Data.Generics.Labels ()
import Data.Image.Types
import Data.Massiv.Array (Sz (..))
import Data.Massiv.Array qualified as M
import Data.Massiv.Array.IO (writeImage)
import Linear
import Linear.Affine (Point (..))
import Linear.Direction
import RIO.FilePath ((</>))
import RayTracing.Camera
import RayTracing.Object.Shape
import RayTracing.Object.Sphere
import RayTracing.Ray
import System.Random
import System.Random.Stateful (runSTGen_)

main :: IO ()
main = do
  g <- getStdGen
  writeImage ("workspace" </> "spheres-antialias.png") $ mkImage g

samplesPerPixel :: Int
samplesPerPixel = 100

aCamera :: Camera
aCamera = mkCamera defaultCameraConfig

mkImage :: RandomGen g => g -> WordImage
mkImage g =
  M.computeP
    $ fromDoubleImage
    $ M.reverse M.Dim2
    $ M.map getAvg
    $ M.foldInner
    $ M.imap
      ( \(M.Ix3 j i _) c ->
          let !u = (fromIntegral i + c) / (fromIntegral imageWidth - 1)
              !v = (fromIntegral j + c) / (fromIntegral imageHeight - 1)
              -- No thin lens here, so we can cheat to pass a dummy StdGen
              !r = runSTGen_ (mkStdGen 42) $ \g' -> getRay g' aCamera $ P $ V2 u v
           in Avg 1 $ colorRay world r
      )
    $ M.computeP @M.U
    $ M.uniformRangeArray g (0.0, 0.999) M.Par (Sz3 imageHeight imageWidth samplesPerPixel)

colorRay :: Hittable obj => obj -> RayColor
colorRay obj r@Ray {..}
  | Just Hit {..} <- hitWithin obj (Just 0) Nothing r =
      let n = unDir normal
       in 0.5 *^ PixelRGB (n ^. _x + 1) (n ^. _y + 1) (n ^. _z + 1)
  | otherwise =
      let !unitDirection = normalize rayDirection
          !t = 0.5 * (unitDirection ^. _y + 1.0)
       in lerp t (PixelRGB 0.5 0.7 1.0) (PixelRGB 1.0 1.0 1.0)

world :: [Sphere]
world = [sphere1, sphere2]

p3 :: (a, a, a) -> Point V3 a
p3 (x, y, z) = P $ V3 x y z

sphere1, sphere2 :: Sphere
sphere1 = Sphere {center = p3 (0, 0, -1), radius = 0.5}
sphere2 = Sphere {center = p3 (0, -100.5, -1), radius = 100}

imageWidth, imageHeight :: Int
imageWidth = 800
imageHeight = floor $ fromIntegral imageWidth / defaultCameraConfig ^. #aspectRatio
