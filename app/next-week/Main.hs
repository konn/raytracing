{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main (main) where

import Control.Applicative ((<**>))
import Control.Lens
import Control.Monad (forM_, when, (<=<))
import Control.Monad.Morph (generalize, hoist)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (State, StateT)
import Control.Monad.Trans.Writer.CPS
import Data.Bifunctor qualified as Bi
import Data.FMList qualified as FML
import Data.Generic.HKD qualified as HKD
import Data.Generics.Labels ()
import Data.Image.Antialiasing (randomSamplingAntialias, stencilAntialiasing)
import Data.Image.Types
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Massiv.Array (Sz (..))
import Data.Massiv.Array qualified as M
import Data.Massiv.Array.IO (readImageAuto, writeImage)
import Data.Massiv.Array.IO qualified as C
import Data.Strict.Tuple (Pair (..))
import Data.Strict.Tuple qualified as STpl
import Data.Traversable (mapAccumL)
import Graphics.ColorModel
import Linear
import Linear.Affine (Point (..))
import Linear.Angle
import Linear.Direction (yDir)
import Math.NumberTheory.Roots (integerSquareRoot)
import Options
import Options.Applicative qualified as Opt
import RIO.FilePath ((</>))
import RIO.FilePath qualified as FP
import RIO.State (MonadState)
import RayTracing.BVH
import RayTracing.Camera
import RayTracing.Ray
import RayTracing.Scene
import RayTracing.Texture
import RayTracing.Texture.Image (ImageTexture' (..), loadImageTexture)
import RayTracing.Texture.Noise.Perlin
import System.Random
import System.Random.Stateful (StateGenM (..), randomRM, runStateGenT_)

default ([])

main :: IO ()
main = do
  opts@Options {..} <- Opt.execParser cmdP
  print opts
  g <- maybe getStdGen (pure . mkStdGen) randomSeed
  let (gScene, g') = split g
  theScene <- runStateGenT_ gScene $ const $ mkScene scene opts
  putStrLn $ "- The scene of " <> show (size $ objects theScene) <> " objects"
  putStrLn $ "- Tree height = " <> show (depth $ objects theScene)
  writeImage outputPath $ mkImage g' opts theScene

toOutputPath :: SceneName -> FilePath
toOutputPath CornellBox = workspace </> "cornell-box" FP.<.> defaultFormat
toOutputPath CornellBoxSmoke = workspace </> "cornell-smoke" FP.<.> defaultFormat
toOutputPath SimpleLight = workspace </> "simple-light" FP.<.> defaultFormat
toOutputPath RandomScene = workspace </> "final" FP.<.> defaultFormat
toOutputPath TwoSpheres = workspace </> "two-spheres" FP.<.> defaultFormat
toOutputPath RayCharles = workspace </> "ray-charles" FP.<.> defaultFormat
toOutputPath Earth = workspace </> "earth" FP.<.> defaultFormat
toOutputPath Perlin = workspace </> "perlin" FP.<.> defaultFormat

defaultFormat :: String
defaultFormat = "png"

workspace :: FilePath
workspace = "workspace"

cmdP :: Opt.ParserInfo Options
cmdP = Opt.info (p <**> Opt.helper) $ Opt.progDesc "Renders spheres with diffusion"
  where
    p =
      Opt.hsubparser $
        mconcat
          [ Opt.command
              "two-spheres"
              (Opt.info (sceneOptionsP TwoSpheres presets) $ Opt.progDesc "Two Spheres example")
          , Opt.command "random" (Opt.info (sceneOptionsP RandomScene presets) $ Opt.progDesc "Two Spheres example")
          , Opt.command "ray-charles" (Opt.info (sceneOptionsP RayCharles presets) $ Opt.progDesc "Random scene in memorial with Ray Charles")
          , Opt.command "earth" (Opt.info (sceneOptionsP Earth presets) $ Opt.progDesc "Earthmap ball")
          , Opt.command "perlin" (Opt.info (sceneOptionsP Perlin presets) $ Opt.progDesc "Play with Perlin noise")
          , Opt.command "simple-light" (Opt.info (sceneOptionsP SimpleLight presets) $ Opt.progDesc "Play with Simple Light")
          , Opt.command "cornell" (Opt.info (sceneOptionsP CornellBox presets) $ Opt.progDesc "Play with Cornell box")
          , Opt.command "cornell-smoke" (Opt.info (sceneOptionsP CornellBoxSmoke presets) $ Opt.progDesc "Play with Cornell box with smoke hexas in it")
          ]

presets :: SceneName -> Defaults
presets scn =
  let def0 = defaultOptions toOutputPath scn
   in case scn of
        RandomScene -> def0 & HKD.field @"imageWidth" .~ pure 500
        RayCharles -> def0 & HKD.field @"imageWidth" .~ pure 500
        TwoSpheres ->
          def0
            & HKD.field @"cameraOrigin" .~ pure (p3 (13, 2, 3))
            & HKD.field @"thinLens" .~ mempty
        Perlin -> def0
        Earth -> def0
        SimpleLight ->
          def0
            & HKD.field @"samplesPerPixel" .~ pure 400
            & HKD.field @"cameraOrigin" .~ pure (p3 (26, 3, 6))
            & HKD.field @"lookingAt" .~ pure (p3 (0, 2, 0))
            & HKD.field @"verticalFieldOfView" .~ pure (20.0 @@ deg)
            & HKD.field @"thinLens" .~ pure Nothing
        CornellBox ->
          def0
            & HKD.field @"binSize" .~ pure 128
            & HKD.field @"bucketSize" .~ pure 8
            & HKD.field @"aspectRatio" .~ pure 1.0
            & HKD.field @"imageWidth" .~ pure 600
            & HKD.field @"samplesPerPixel" .~ pure 200
            & HKD.field @"cameraOrigin" .~ pure (p3 (278, 278, -800))
            & HKD.field @"lookingAt" .~ pure (p3 (278, 278, 0))
            & HKD.field @"verticalFieldOfView" .~ pure (40.0 @@ deg)
            & HKD.field @"thinLens" .~ pure Nothing
        CornellBoxSmoke ->
          def0
            & HKD.field @"binSize" .~ pure 128
            & HKD.field @"bucketSize" .~ pure 8
            & HKD.field @"aspectRatio" .~ pure 1.0
            & HKD.field @"imageWidth" .~ pure 600
            & HKD.field @"samplesPerPixel" .~ pure 200
            & HKD.field @"cameraOrigin" .~ pure (p3 (278, 278, -800))
            & HKD.field @"lookingAt" .~ pure (p3 (278, 278, 0))
            & HKD.field @"verticalFieldOfView" .~ pure (40.0 @@ deg)
            & HKD.field @"thinLens" .~ pure Nothing

mkImage :: RandomGen g => g -> Options -> Scene -> WordImage
mkImage g0 Options {..} theScene =
  let imageHeight =
        floor $ fromIntegral imageWidth / aspectRatio
      sz = Sz2 imageHeight imageWidth
      aCamera =
        mkCamera $
          defaultCameraConfig
            & #aspectRatio .~ aspectRatio
            & #verticalFieldOfView .~ verticalFieldOfView
            & #cameraOrigin .~ cameraOrigin
            & #lookingAt .~ lookingAt
            & #viewUp .~ viewUp
            & #thinLens .~ thinLens
      antialias = case antialiasing of
        Random -> randomSamplingAntialias g0 samplesPerPixel sz
        Stencil -> stencilAntialiasing g0 (integerSquareRoot samplesPerPixel) sz
   in M.computeP $
        fromDoubleImage $
          correctGamma $
            antialias $
              curry $
                rayColour epsilon theScene cutoff
                  <=< getRay aCamera . p2

p2 :: (a, a) -> Point V2 a
p2 = P . uncurry V2

mkScene :: RandomGen g => SceneName -> Options -> StateT g IO Scene
mkScene CornellBoxSmoke Options {..} = do
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
      box1 =
        Box (p3 (0, 0, 0)) (p3 (165, 330, 165))
          & Rotate (axisAngleA yDir (15 @@ deg))
          & Translate (V3 265 0 295)
          & ConstantMedium 0.01
      box2 =
        Box (p3 (0, 0, 0)) (p3 (165, 165, 165))
          & Rotate (axisAngleA yDir (-18 @@ deg))
          & Translate (V3 130 0 65)
          & ConstantMedium 0.005
      objs =
        [ mkSomeObject leftWall green
        , mkSomeObject rightWall red
        , mkSomeObject ceilLight light
        , mkSomeObject bottomWall white
        , mkSomeObject topWall white
        , mkSomeObject rearWall white
        , mkSomeObject box1 $ Isotropic $ ColorRGB 0 0 0
        , mkSomeObject box2 $ Isotropic $ ColorRGB 1 1 1
        ]
  objects <- hoist generalize $ fromObjectsWithBinBucket binSize bucketSize objs
  pure Scene {objects, background = const 0}
mkScene CornellBox Options {..} = do
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
      box1 =
        Box (p3 (0, 0, 0)) (p3 (165, 330, 165))
          & Rotate (axisAngleA yDir (15 @@ deg))
          & Translate (V3 265 0 295)
      box2 =
        Box (p3 (0, 0, 0)) (p3 (165, 165, 165))
          & Rotate (axisAngleA yDir (-18 @@ deg))
          & Translate (V3 130 0 65)
      objs =
        [ mkSomeObject leftWall green
        , mkSomeObject rightWall red
        , mkSomeObject ceilLight light
        , mkSomeObject bottomWall white
        , mkSomeObject topWall white
        , mkSomeObject rearWall white
        , mkSomeObject box1 white
        , mkSomeObject box2 white
        ]
  objects <- hoist generalize $ fromObjectsWithBinBucket binSize bucketSize objs
  pure Scene {objects, background = const 0}
mkScene SimpleLight Options {..} = do
  perlinSeed <- hoist generalize randomPerlinSeed
  let pertext =
        Lambertian
          PerlinMerble
            { turbulenceScale = 10
            , turbulenceDepth = 7
            , perlinSeed = perlinSeed
            , coordPhase = 4
            }
      ground = Sphere {radius = 1000, center = p3 (0, -1000, 0)}
      ball = Sphere {radius = 2, center = p3 (0, 2, 0)}
      light = DiffuseLight $ MkAttn 4 4 4
      rect = xyPlane (-2) (V2 (3, 5) (1, 3))
      lamp = Sphere {radius = 1.5, center = p3 (0, 6.5, 0)}
      blueLight = DiffuseLight $ 4 *^ ColorRGB 0.0 0.125 0.5
      objs =
        [ mkSomeObject ground pertext
        , mkSomeObject ball pertext
        , mkSomeObject rect light
        , mkSomeObject lamp blueLight
        ]
  objects <- hoist generalize $ fromObjectsWithBinBucket binSize bucketSize objs
  pure Scene {objects, background = const 0}
mkScene Perlin Options {..} = do
  perlinSeed <- hoist generalize randomPerlinSeed
  let pertext =
        Lambertian
          PerlinMerble
            { turbulenceDepth = 7
            , turbulenceScale = 10.0
            , coordPhase = 4.0 *^ normalize (V3 0 1.0 1.0)
            , ..
            }
      sph = Sphere {center = p3 (0, 2, 0), radius = 2}
      ground = Sphere {radius = 1000, center = p3 (0, -1000, 0)}
      objs = [mkSomeObject ground pertext, mkSomeObject sph pertext]
  objects <- hoist generalize $ fromObjectsWithBinBucket binSize bucketSize objs
  pure
    Scene
      { objects
      , background = blueGradientBackground
      }
mkScene Earth Options {..} = do
  earthmap <- loadImageTexture $ "data" </> "earthmap.jpg"
  let earth = Lambertian earthmap
      sph1 = Sphere {center = p3 (0, 0, 0), radius = 2}
      objs = [mkSomeObject sph1 earth]
  objects <- hoist generalize $ fromObjectsWithBinBucket binSize bucketSize objs
  pure
    Scene
      { objects
      , background = blueGradientBackground
      }
mkScene TwoSpheres Options {..} = do
  let checker =
        Lambertian $
          CheckerTexture
            (ColorRGB 0.2 0.3 0.1)
            (ColorRGB 0.9 0.9 0.9)
            10
            10
            10
      sph1 = Sphere {center = p3 (0, -10, 0), radius = 10}
      sph2 = Sphere {center = p3 (0, 10, 0), radius = 10}
      objs = [mkSomeObject sph1 checker, mkSomeObject sph2 checker]
  objects <- hoist generalize $ fromObjectsWithBinBucket binSize bucketSize objs
  pure
    Scene
      { objects
      , background = blueGradientBackground
      }
mkScene RandomScene Options {..} = do
  let ground = Sphere {center = p3 (0, -1000, 0), radius = 1000}
      groundMaterial =
        Lambertian $
          CheckerTexture (ColorRGB 0.2 0.3 0.1) (ColorRGB 0.9 0.9 0.9) 10 10 10
      material1 = Dielectric 1.5
      sphere1 = Sphere (p3 (0, 1, 0)) 1.0
      material2 = Lambertian $ MkAttn 0.4 0.2 0.1
      sphere2 = Sphere (p3 (-4, 1, 0)) 1.0
      material3 = Metal $ MkAttn 0.7 0.6 0.5
      sphere3 = Sphere (p3 (4, 1, 0)) 1.0
  balls <- hoist generalize $ execWriterT generateBalls
  let objs =
        FML.cons
          (mkSomeObject ground groundMaterial)
          balls
          <> FML.fromList
            [ mkSomeObject sphere1 material1
            , mkSomeObject sphere2 material2
            , mkSomeObject sphere3 material3
            ]
  !bvh <- hoist generalize $ fromObjectsWithBinBucket binSize bucketSize objs
  pure
    Scene
      { objects = bvh
      , background = blueGradientBackground
      }
mkScene RayCharles Options {..} = do
  charlesGS <-
    readImageAuto @M.S @(C.Y' C.SRGB) @Double $
      "data" </> "ray-charles.jpg"
  let ground = Sphere {center = p3 (0, -1000, 0), radius = 1000}
      groundMaterial =
        Lambertian $
          CheckerTexture (ColorRGB 0.2 0.3 0.1) (ColorRGB 0.9 0.9 0.9) 10 10 10
      material1 = Dielectric 1.5
      sphere1 = Sphere (p3 (0, 1, 0)) 1.0
      material2 = Lambertian $ MkAttn 0.4 0.2 0.1
      sphere2 = Sphere (p3 (-4, 1, 0)) 1.0
      imgTxt =
        ImageTexture $
          M.computeP @M.S $
            M.map (\(C.PixelY' x) -> x *^ PixelRGB 0.7 0.6 0.5) charlesGS
      material3 =
        Metal
          TextureOffset {offset = V2 0.025 0.1, texture = imgTxt}
      sphere3 = Sphere (p3 (4, 1, 0)) 1.0
  balls <- hoist generalize $ execWriterT generateBalls
  let objs =
        FML.cons
          (mkSomeObject ground groundMaterial)
          balls
          <> FML.fromList
            [ mkSomeObject sphere1 material1
            , mkSomeObject sphere2 material2
            , mkSomeObject sphere3 material3
            ]
  !bvh <- hoist generalize $ fromObjectsWithBinBucket binSize bucketSize objs
  pure
    Scene
      { objects = bvh
      , background = blueGradientBackground
      }

blueGradientBackground :: Ray -> Pixel RGB Double
blueGradientBackground Ray {..} =
  let !unitDirection = normalize rayDirection
      !t = 0.5 * (unitDirection ^. _y + 1.0)
   in lerp t (PixelRGB 0.5 0.7 1.0) (PixelRGB 1.0 1.0 1.0)

randomAtten :: (RandomGen g, MonadState g m) => (Double, Double) -> m (Attenuation RGB Double)
randomAtten ran = sequenceA $ pure $ randomRM ran StateGenM

chooseM :: (RandomGen g, MonadState g m) => NonEmpty (Double, m a) -> m a
chooseM alts = do
  let (count :!: total, wps) =
        mapAccumL
          ( \(!l :!: !acc) (p, m) ->
              let !len = l + 1
                  !tot' = acc + p
               in (len :!: tot', tot' :!: m)
          )
          (0 :!: 0)
          alts
      (wps', w) = Bi.second (STpl.snd . head) $ NE.splitAt (count - 1) wps
  weight <- randomRM (0, total) StateGenM
  foldr
    (\(thresh :!: m) alt -> if weight <= thresh then m else alt)
    w
    wps'

p3 :: (a, a, a) -> Point V3 a
p3 (x, y, z) = P $ V3 x y z

generateBalls ::
  RandomGen g =>
  WriterT
    (FML.FMList SomeObject)
    (State g)
    ()
generateBalls = forM_ @[] [-11 .. 10] $ \a -> forM_ @[] [-11 .. 10] $ \b -> do
  r <- lift $ randomRM (0.15, 0.25) StateGenM
  let basePt = p3 (4, r, 0.0)
  dx <- lift $ randomRM (0, 0.9) StateGenM
  dy <- lift $ randomRM (0, 0.9) StateGenM
  let center = p3 (a + dx, r, b + dy)
      sphere = Sphere center r
  when (distance center basePt > 0.9) $ do
    objects <-
      lift $
        chooseM
          [
            ( 7.5
            , pure . mkSomeObject sphere . Lambertian
                <$> ((*) <$> randomAtten (0, 1.0) <*> randomAtten (0, 1.0))
            )
          ,
            ( 1.0
            , fmap (pure . mkSomeObject sphere) . FuzzyMetal
                <$> randomAtten (0.5, 1.0)
                <*> randomRM (0, 0.5) StateGenM
            )
          ,
            ( 0.1
            , do
                glass <- Dielectric <$> randomRM (1.5, 1.7) StateGenM
                pure [mkSomeObject sphere glass]
            )
          ,
            ( 0.05
            , do
                glass <- Dielectric <$> randomRM (1.5, 1.7) StateGenM
                pure
                  [ mkSomeObject sphere glass
                  , mkSomeObject (sphere & #radius *~ -0.9) glass
                  ]
            )
          ]
    tell $ FML.fromList objects
