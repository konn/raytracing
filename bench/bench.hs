{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Lens ((^.))
import Data.Function ((&))
import Data.Image.Antialiasing
import Data.Image.Types
import Data.Massiv.Array (Sz (..))
import Data.Massiv.Array qualified as M
import GHC.Generics (Generic)
import Graphics.ColorModel
import Linear
import Linear.Affine
import Linear.Angle
import Linear.Direction
import Math.NumberTheory.Roots (integerSquareRoot)
import RayTracing.Camera
import RayTracing.Ray
import RayTracing.Scene
import System.Random.Stateful
import Test.Tasty (defaultMainWithIngredients, localOption, mkTimeout)
import Test.Tasty.Bench
import Test.Tasty.Runners (NumThreads (NumThreads))

main :: IO ()
main =
  defaultMainWithIngredients benchIngredients $
    localOption (mkTimeout $ 90 * 10 ^ (6 :: Int)) $
      localOption (NumThreads 1) $
        localOption WallTime $
          bgroup "All" benchmarks

benchmarks :: [Benchmark]
benchmarks =
  [ bgroup
      "metal-spheres"
      [ bgroup
        ("width = " <> show sz)
        [ bench "random-antialias" $ nf (mkMetalSpheres sz theGen) Random
        , bench "stencil-antialias" $ nf (mkMetalSpheres sz theGen) Stencil
        ]
      | sz <- [40, 100, 200]
      ]
  , bgroup
      "cornell-box-solid"
      [ bgroup
        ("width = " <> show sz)
        [ bench "random-antialias" $ nf (mkCornellBox Solid sz theGen) Random
        , bench "stencil-antialias" $ nf (mkCornellBox Solid sz theGen) Stencil
        ]
      | sz <- [50, 100, 200, 400]
      ]
  , bgroup
      "cornell-box-smoke"
      [ bgroup
        ("width = " <> show sz)
        [ bench "random-antialias" $ nf (mkCornellBox Smoke sz theGen) Random
        , bench "stencil-antialias" $ nf (mkCornellBox Smoke sz theGen) Stencil
        ]
      | sz <- [50, 100, 200, 400]
      ]
  ]

aCamera :: Camera
aCamera = mkCamera defaultCameraConfig

data Antialiasing = Random | Stencil
  deriving (Show, Eq, Ord)

theGen :: StdGen
theGen = mkStdGen 42

samplesPerPixel :: Int
samplesPerPixel = 64

mkMetalSpheres :: (RandomGen g) => Int -> g -> Antialiasing -> WordImage
{-# INLINE mkMetalSpheres #-}
mkMetalSpheres = mkImage metalSphereScene

cutoff :: Int
cutoff = 50

epsilon :: Double
epsilon = 0.001

p2 :: (a, a) -> Point V2 a
p2 = P . uncurry V2

metalSphereScene :: Scene
metalSphereScene =
  let ground = Sphere {center = p3 (0, -100.5, -1), radius = 100}
      groundMaterial = Lambertian $ MkAttn 0.8 0.8 0.0
      centerMaterial = Lambertian $ MkAttn 0.7 0.3 0.3
      center = Sphere {center = p3 (0, 0, -1), radius = 0.5}
      leftRatio = MkAttn 0.8 0.8 0.8
      leftMaterial = MkSomeMaterial $ FuzzyMetal leftRatio 0.3
      leftS = Sphere {center = p3 (-1, 0, -1), radius = 0.5}
      rightRatio = MkAttn 0.8 0.6 0.2
      rightMaterial = MkSomeMaterial $ Metal rightRatio
      rightS = Sphere {center = p3 (1, 0, -1), radius = 0.5}
   in Scene
        { objects =
            toFlatBVH
              [ mkSomeObject ground groundMaterial
              , mkSomeObject center centerMaterial
              , mkSomeObject leftS leftMaterial
              , mkSomeObject rightS rightMaterial
              ]
        , background = \Ray {..} ->
            let !unitDirection = normalize rayDirection
                !t = 0.5 * (unitDirection ^. _y + 1.0)
             in lerp t (PixelRGB 0.5 0.7 1.0) (PixelRGB 1.0 1.0 1.0)
        }

p3 :: (a, a, a) -> Point V3 a
{-# INLINE p3 #-}
p3 (x, y, z) = P $ V3 x y z

data CornellMode = Solid | Smoke
  deriving (Show, Eq, Ord, Generic)

mkCornellBox :: (RandomGen g) => CornellMode -> Int -> g -> Antialiasing -> WordImage
{-# INLINE mkCornellBox #-}
mkCornellBox = mkImage . cornellBoxScene

cornellBoxScene :: CornellMode -> Scene
{-# INLINE cornellBoxScene #-}
cornellBoxScene mode =
  let red = Lambertian $ ColorRGB 0.65 0.05 0.05
      white = Lambertian $ ColorRGB 0.73 0.73 0.73
      green = Lambertian $ ColorRGB 0.12 0.45 0.15
      light = DiffuseLight $ ColorRGB 15 15 15
      leftWall = yzPlane 555 (V2 (0, 555) (0, 555))
      rightWall = yzPlane 0 (V2 (0, 555) (0, 555))
      ceilLight = zxPlane 554 (V2 (227, 332) (213, 343))
      bottomWall = zxPlane 0 (V2 (0, 555) (0, 555))
      topWall = zxPlane 555 (V2 (0, 555) (0, 555))
      rearWall = xyPlane 555 (V2 (0, 555) (0, 555))
      binSize = 128
      bucketSize = 8
      (box1, box2) =
        case mode of
          Smoke ->
            let b1 =
                  Box (p3 (0, 0, 0)) (p3 (165, 330, 165))
                    & Rotate (axisAngleA yDir (15 @@ deg))
                    & Translate (V3 265 0 295)
                    & ConstantMedium 0.01
                b2 =
                  Box (p3 (0, 0, 0)) (p3 (165, 165, 165))
                    & Rotate (axisAngleA yDir (-18 @@ deg))
                    & Translate (V3 130 0 65)
                    & ConstantMedium 0.005
             in ( mkSomeObject b1 $ Isotropic $ ColorRGB 0 0 0
                , mkSomeObject b2 $ Isotropic $ ColorRGB 1 1 1
                )
          Solid ->
            let b1 =
                  Box (p3 (0, 0, 0)) (p3 (165, 330, 165))
                    & Rotate (axisAngleA yDir (15 @@ deg))
                    & Translate (V3 265 0 295)
                b2 =
                  Box (p3 (0, 0, 0)) (p3 (165, 165, 165))
                    & Rotate (axisAngleA yDir (-18 @@ deg))
                    & Translate (V3 130 0 65)
             in ( mkSomeObject b1 $ Lambertian $ ColorRGB 0 0 0
                , mkSomeObject b2 $ Lambertian $ ColorRGB 1 1 1
                )
      objects =
        fromObjectsWithBinBucket
          binSize
          bucketSize
          [ mkSomeObject leftWall green
          , mkSomeObject rightWall red
          , mkSomeObject ceilLight light
          , mkSomeObject bottomWall white
          , mkSomeObject topWall white
          , mkSomeObject rearWall white
          , box1
          , box2
          ]
   in Scene {objects, background = const 0}

mkImage :: (RandomGen g) => Scene -> Int -> g -> Antialiasing -> WordImage
mkImage scene imageWidth g0 antialiasing =
  let imageHeight =
        floor @Double $ fromIntegral imageWidth / (16.0 / 9.0)
      sz = Sz2 imageHeight imageWidth
      antialias = case antialiasing of
        Random -> randomSamplingAntialias g0 samplesPerPixel sz
        Stencil -> stencilAntialiasing g0 (integerSquareRoot samplesPerPixel) sz
   in M.computeP $
        fromDoubleImage $
          correctGamma $
            antialias $ \u v ->
              rayColour epsilon scene cutoff
                =<< getRay aCamera (p2 (u, v))
