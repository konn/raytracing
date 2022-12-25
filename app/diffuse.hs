{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Control.Arrow ((<<<))
import Control.Lens
import Data.Generics.Labels ()
import Data.Image.Format.PPM
import Data.Image.Types
import Data.Massiv.Array (Sz (..))
import Data.Massiv.Array qualified as M
import Data.Vector.Unboxed.Deriving (derivingUnbox)
import Linear
import Linear.Affine (Point (..), (.+^), (.-.))
import Linear.Direction
import RIO.FilePath ((</>))
import RayTracing.Camera
import RayTracing.Object.Classes
import RayTracing.Object.Sphere
import RayTracing.Ray
import System.Random
import System.Random.Stateful (RandomGenM (..), randomRM, runSTGen)
import System.Random.Utils (randomPointInUnitSphere)

data Avg = Avg !Word !(Pixel Double)
  deriving (Show, Eq, Ord)

derivingUnbox
  "Avg"
  [t|Avg -> (Word, Pixel Double)|]
  [|\(Avg c s) -> (c, s)|]
  [|uncurry Avg|]

instance Semigroup Avg where
  Avg cntl suml <> Avg cntr sumr = Avg (cntl + cntr) (suml ^+^ sumr)
  {-# INLINE (<>) #-}

instance Monoid Avg where
  mempty = Avg 0 $ Pixel 0 0 0
  {-# INLINE mempty #-}

getAvg :: Avg -> Pixel Double
getAvg (Avg cnt total) = total ^/ fromIntegral cnt

main :: IO ()
main = do
  g <- getStdGen
  writePPMFile ("workspace" </> "diffuse.ppm") $ mkImage g

samplesPerPixel :: Int
samplesPerPixel = 100

aCamera :: Camera
aCamera = mkCamera defaultCameraConfig

mkImage :: RandomGen g => g -> WordImage
mkImage g0 =
  fromDoubleImage $
    M.computeP $
      correctGamma $
        M.reverse M.Dim2 $
          M.map getAvg $
            M.foldInner $
              view _3 $
                M.generateSplitSeedArray @M.U
                  M.defRowMajorUnbalanced
                  g0
                  (pure . split)
                  M.Par
                  (Sz3 imageHeight imageWidth samplesPerPixel)
                  ( \_ (M.Ix3 j i _) ->
                      pure <<< flip runSTGen \g -> do
                        !c <- randomRM (0, 1.0) g
                        let !u = (fromIntegral i + c) / (fromIntegral imageWidth - 1)
                            !v = (fromIntegral j + c) / (fromIntegral imageHeight - 1)
                            !r = getRay aCamera $ P $ V2 u v
                        Avg 1 <$> colorRayDiffuse world g 100 r
                  )

epsilon :: Double
epsilon = 0.01

colorRayDiffuse :: (RandomGenM g r m, Hittable obj) => obj -> g -> Int -> Ray -> m (Pixel Double)
colorRayDiffuse obj g = go
  where
    {-# INLINE go #-}
    go !depth r@Ray {..}
      | depth <= 0 = pure $ Pixel 0 0 0
      | Just Hit {..} <- hitWithin (Just epsilon) Nothing r obj = do
          dev <- applyRandomGenM randomPointInUnitSphere g
          let n = unDir normal
              target = coord .+^ n .+^ unDir dev
              reflected =
                Ray
                  { rayOrigin = coord
                  , rayDirection = target .-. coord
                  }
           in (0.5 *^) <$> go (depth - 1) reflected
      | otherwise = do
          let !unitDirection = normalize rayDirection
              !t = 0.5 * (unitDirection ^. _y + 1.0)
          pure $ lerp t (Pixel 0.5 0.7 1.0) (Pixel 1.0 1.0 1.0)

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
