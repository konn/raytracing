{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
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

import Control.Applicative ((<**>), (<|>))
import Control.Lens
import Control.Monad (forM_, guard, when, (<=<))
import Control.Monad.ST.Strict (ST)
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Trans.Writer.CPS
import Data.Bifunctor qualified as Bi
import Data.ByteString.Char8 qualified as BS
import Data.Char qualified as C
import Data.FMList qualified as FML
import Data.Generics.Labels ()
import Data.Image.Antialiasing (randomSamplingAntialias, stencilAntialiasing)
import Data.Image.Types
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Massiv.Array (Sz (..))
import Data.Massiv.Array qualified as M
import Data.Massiv.Array.IO (writeImage)
import Data.Maybe (fromMaybe)
import Data.Monoid (Alt (..))
import Data.Scientific qualified as S
import Data.Strict.Tuple (Pair (..))
import Data.Strict.Tuple qualified as STpl
import Data.Traversable (mapAccumL)
import Data.Trie qualified as Trie
import GHC.Generics (Generic)
import Graphics.ColorModel
import Linear
import Linear.Affine (Point (..))
import Linear.Angle (Angle, deg, (@@))
import Linear.Direction (Dir, dir)
import Math.NumberTheory.Roots (integerSquareRoot)
import Numeric.Natural (Natural)
import Options.Applicative qualified as Opt
import RIO.FilePath ((</>))
import RIO.FilePath qualified as FP
import RIO.Text qualified as T
import RIO.Text.Partial qualified as T
import RayTracing.BVH
import RayTracing.Camera
import RayTracing.Object hiding (Scene, SceneOf (..), rayColour)
import RayTracing.Object.Sphere
import RayTracing.Ray
import RayTracing.Texture
import System.Random
import System.Random.Orphans ()
import System.Random.Stateful (RandomGenM, STGenM, applyRandomGenM, randomRM, runSTGen_)
import Text.Read (readMaybe)

default ([])

main :: IO ()
main = do
  opts@Options {..} <- Opt.execParser cmdP
  g <- maybe getStdGen (pure . mkStdGen) randomSeed
  let (gScene, g') = split g
      theScene = runSTGen_ gScene (mkScene scene opts)
  putStrLn $ "- The scene of " <> show (size $ objects theScene) <> " objects"
  putStrLn $ "- Tree height = " <> show (depth $ objects theScene)
  writeImage (fromMaybe (toOutputPath scene) outputPath) $ mkImage g' opts theScene

toOutputPath :: SceneName -> FilePath
toOutputPath RandomScene = workspace </> "final" FP.<.> defaultFormat
toOutputPath TwoSpheres = workspace </> "two-spheres" FP.<.> defaultFormat

defaultFormat :: String
defaultFormat = "png"

workspace :: FilePath
workspace = "workspace"

data Diffusion = Lambert | Hemisphere
  deriving (Show, Eq, Ord, Generic)

data Antialiasing = Random | Stencil
  deriving (Show, Eq, Ord, Generic)

data Options = Options
  { cutoff :: !Int
  , samplesPerPixel :: !Int
  , imageWidth :: !Int
  , epsilon :: !Double
  , outputPath :: !(Maybe FilePath)
  , antialiasing :: !Antialiasing
  , aspectRatio :: !Double
  , verticalFieldOfView :: !(Angle Double)
  , cameraOrigin :: !(Point V3 Double)
  , lookingAt :: !(Point V3 Double)
  , viewUp :: !(Dir V3 Double)
  , thinLens :: !(Maybe ThinLens)
  , bucketSize :: !Int
  , randomSeed :: !(Maybe Int)
  , scene :: !SceneName
  }
  deriving (Show, Eq, Ord, Generic)

data SceneName = RandomScene | TwoSpheres
  deriving (Show, Eq, Ord, Generic)

cmdP :: Opt.ParserInfo Options
cmdP = Opt.info (p <**> Opt.helper) $ Opt.progDesc "Renders spheres with diffusion"
  where
    p = do
      scene <-
        Opt.hsubparser $
          mconcat
            [ Opt.command "two-spheres" (Opt.info (pure TwoSpheres) $ Opt.progDesc "Two Spheres example")
            , Opt.command "random" (Opt.info (pure RandomScene) $ Opt.progDesc "Two Spheres example")
            ]
      cutoff <-
        fromIntegral @Natural
          <$> Opt.option
            Opt.auto
            ( Opt.long "cutoff"
                <> Opt.short 'M'
                <> Opt.metavar "NUM"
                <> Opt.value 50
                <> Opt.showDefault
                <> Opt.help "Maximum iteration to trace the diffusion"
            )
      samplesPerPixel <-
        fromIntegral @Natural
          <$> Opt.option
            Opt.auto
            ( Opt.long "samples"
                <> Opt.short 's'
                <> Opt.metavar "NUM"
                <> Opt.value 500
                <> Opt.showDefault
                <> Opt.help "Sample size for randomised antialiasing"
            )
      imageWidth <-
        fromIntegral @Natural
          <$> Opt.option
            Opt.auto
            ( Opt.long "width"
                <> Opt.short 'w'
                <> Opt.metavar "NUM"
                <> Opt.value 1200
                <> Opt.showDefault
                <> Opt.help "Sample size for randomised antialiasing"
            )
      epsilon <-
        Opt.option (Opt.auto >>= \d -> d <$ guard (d >= 0)) $
          Opt.long "epsilon"
            <> Opt.short 'E'
            <> Opt.value 0.001
            <> Opt.showDefault
            <> Opt.help "The threshold to regard as zero"
      outputPath <-
        Opt.optional $
          Opt.strOption $
            Opt.long "output"
              <> Opt.short 'o'
              <> Opt.metavar "PATH"
              <> Opt.help "Output path"
      antialiasing <-
        Opt.option (Opt.maybeReader parseAntialising) $
          Opt.long "antialias"
            <> Opt.short 'A'
            <> Opt.value Random
            <> Opt.showDefault
            <> Opt.help "Antialiasing method"
      aspectRatio <-
        fromRational
          <$> Opt.option
            (Opt.maybeReader parseRatio)
            ( Opt.long "aspect"
                <> Opt.value (3 / 2)
                <> Opt.showDefault
                <> Opt.help "Aspect ratio"
            )
      verticalFieldOfView <-
        (@@ deg)
          <$> Opt.option
            Opt.auto
            ( Opt.long "vfov"
                <> Opt.short 'V'
                <> Opt.long "vertical-fov"
                <> Opt.value 20.0
                <> Opt.help "Vertical field of view, in Degree"
                <> Opt.showDefault
            )
      cameraOrigin <-
        fmap p3 $
          Opt.option Opt.auto $
            Opt.long "camera-origin"
              <> Opt.short 'O'
              <> Opt.value (13, 2, 3)
              <> Opt.showDefault
              <> Opt.help "The camera origin where the camera looks from"
      lookingAt <-
        fmap p3 $
          Opt.option Opt.auto $
            Opt.long "look-at"
              <> Opt.short 'F'
              <> Opt.value (0, 0, 0)
              <> Opt.showDefault
              <> Opt.help "The point that the camera is looking at"
      viewUp <-
        Opt.option
          Opt.auto
          ( Opt.long "view-up"
              <> Opt.long "vup"
              <> Opt.short 'U'
              <> Opt.value (0, 1, 0)
              <> Opt.help "The View-Up vector of the camera"
          )
          <&> \(x, y, z) -> dir (V3 x y z)
      thinLens <-
        Opt.flag' Nothing (Opt.long "no-lens" <> Opt.help "renders without a lens")
          <|> Just <$> thinLensP
      randomSeed <-
        Opt.optional $
          Opt.option Opt.auto $
            Opt.long "seed"
              <> Opt.short 'S'
              <> Opt.help "If specified, use the number as the seed for the random number"
      bucketSize <-
        Opt.option Opt.auto $
          Opt.long "bucket-size"
            <> Opt.short 'B'
            <> Opt.value 8
            <> Opt.showDefault
            <> Opt.help "The maximum # of objects to store in a BVH Node"
      pure Options {..}

thinLensP :: Opt.Parser ThinLens
thinLensP = do
  aperture <-
    Opt.option Opt.auto $
      Opt.long "aperture" <> Opt.short 'a' <> Opt.help "The aperture of a camera lens" <> Opt.showDefault <> Opt.value 0.1
  focusDistance <-
    Opt.option Opt.auto $
      Opt.long "focus-distance"
        <> Opt.long "fdist"
        <> Opt.short 'D'
        <> Opt.help "The focus distance of a camera lens. If --aperture is specified and this option is omitted, it will be set to the distance from the camera origin to looking-at point."
        <> Opt.value 10.0
        <> Opt.showDefault
  pure ThinLens {..}

parseRatio :: String -> Maybe Rational
parseRatio =
  runReaderT $
    getAlt $
      foldMap @[]
        (Alt . ReaderT)
        [ readMaybe . T.unpack . T.replace "/" "%" . T.pack
        , fmap (toRational . S.toRealFloat @Double) . readMaybe
        ]

parseAntialising :: String -> Maybe Antialiasing
parseAntialising = flip (Trie.lookupBy go) dic . BS.pack . map C.toLower
  where
    go (Just m) _ = Just m
    go Nothing sub
      | Trie.null sub = Nothing
      | [a] <- Trie.elems sub = Just a
      | otherwise = Nothing
    dic =
      Trie.fromList
        [ (BS.pack $ map C.toLower $ show mtd, mtd)
        | mtd <- [Random, Stencil]
        ]

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
            antialias $ \g ->
              curry $
                rayColour epsilon theScene g cutoff
                  <=< getRay g aCamera . p2

p2 :: (a, a) -> Point V2 a
p2 = P . uncurry V2

mkScene :: RandomGen g => SceneName -> Options -> STGenM g s -> ST s Scene
mkScene TwoSpheres Options {..} g = do
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
      objs = [MkSomeObject sph1 checker, MkSomeObject sph2 checker]
  objects <- applyRandomGenM (fromObjectsWithBucket bucketSize objs) g
  pure
    Scene
      { objects
      , background = blueGradientBackground
      }
mkScene RandomScene Options {..} g = do
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
  balls <- execWriterT generateBalls
  let objs =
        FML.cons
          (MkSomeObject ground groundMaterial)
          balls
          <> FML.fromList
            [ MkSomeObject sphere1 material1
            , MkSomeObject sphere2 material2
            , MkSomeObject sphere3 material3
            ]
  !bvh <- applyRandomGenM (fromObjectsWithBucket bucketSize objs) g
  pure
    Scene
      { objects = bvh
      , background = blueGradientBackground
      }
  where
    generateBalls = forM_ @[] [-11 .. 10] $ \a -> forM_ @[] [-11 .. 10] $ \b -> do
      r <- randomRM (0.15, 0.25) g
      let basePt = p3 (4, r, 0.0)
      dx <- randomRM (0, 0.9) g
      dy <- randomRM (0, 0.9) g
      let center = p3 (a + dx, r, b + dy)
          sphere = Sphere center r
      when (distance center basePt > 0.9) $ do
        objects <-
          chooseM
            g
            [
              ( 7.5
              , pure . MkSomeObject sphere . Lambertian
                  <$> ((*) <$> randomAtten (0, 1.0) g <*> randomAtten (0, 1.0) g)
              )
            ,
              ( 1.0
              , fmap (pure . MkSomeObject sphere) . FuzzyMetal
                  <$> randomAtten (0.5, 1.0) g
                  <*> randomRM (0, 0.5) g
              )
            ,
              ( 0.1
              , do
                  glass <- Dielectric <$> randomRM (1.5, 1.7) g
                  pure [MkSomeObject sphere glass]
              )
            ,
              ( 0.05
              , do
                  glass <- Dielectric <$> randomRM (1.5, 1.7) g
                  pure
                    [ MkSomeObject sphere glass
                    , MkSomeObject (sphere & #radius *~ -0.9) glass
                    ]
              )
            ]
        tell $ FML.fromList objects

blueGradientBackground :: Ray -> Pixel RGB Double
blueGradientBackground Ray {..} =
  let !unitDirection = normalize rayDirection
      !t = 0.5 * (unitDirection ^. _y + 1.0)
   in lerp t (PixelRGB 0.5 0.7 1.0) (PixelRGB 1.0 1.0 1.0)

randomAtten :: RandomGenM g r m => (Double, Double) -> g -> m (Attenuation RGB Double)
randomAtten ran = sequenceA . pure . randomRM ran

chooseM :: RandomGenM g r m => g -> NonEmpty (Double, m a) -> m a
chooseM g alts = do
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
  weight <- randomRM (0, total) g
  foldr
    (\(thresh :!: m) alt -> if weight <= thresh then m else alt)
    w
    wps'

p3 :: (a, a, a) -> Point V3 a
p3 (x, y, z) = P $ V3 x y z
