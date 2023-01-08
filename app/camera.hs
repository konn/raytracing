{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main (main) where

import Control.Applicative ((<**>), (<|>))
import Control.Lens
import Control.Monad (guard, (<=<))
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.ByteString.Char8 qualified as BS
import Data.Char qualified as C
import Data.Generics.Labels ()
import Data.Image.Antialiasing (randomSamplingAntialias, stencilAntialiasing)
import Data.Image.Types
import Data.Massiv.Array (Sz (..))
import Data.Massiv.Array qualified as M
import Data.Massiv.Array.IO hiding (PixelRGB)
import Data.Maybe (fromMaybe)
import Data.Monoid (Alt (..))
import Data.Scientific qualified as S
import Data.Trie qualified as Trie
import GHC.Generics (Generic)
import Linear
import Linear.Affine (Point (..))
import Linear.Angle (Angle, deg, (@@))
import Linear.Direction (Dir, dir)
import Math.NumberTheory.Roots (integerSquareRoot)
import Numeric.Natural (Natural)
import Options.Applicative qualified as Opt
import RIO.FilePath ((</>))
import RIO.Text qualified as T
import RIO.Text.Partial qualified as T
import RayTracing.BVH
import RayTracing.Camera
import RayTracing.Ray
import RayTracing.Scene
import System.Random
import Text.Read (readMaybe)

main :: IO ()
main = do
  opts@Options {..} <- Opt.execParser cmdP
  g <- getStdGen
  writeImage outputPath $ mkImage g opts

data Diffusion = Lambert | Hemisphere
  deriving (Show, Eq, Ord, Generic)

data Antialiasing = Random | Stencil
  deriving (Show, Eq, Ord, Generic)

data Options = Options
  { cutoff :: !Int
  , samplesPerPixel :: !Int
  , hollow :: !Bool
  , imageWidth :: !Int
  , epsilon :: !Double
  , outputPath :: !FilePath
  , antialiasing :: !Antialiasing
  , aspectRatio :: !Double
  , verticalFieldOfView :: !(Angle Double)
  , cameraOrigin :: !(Point V3 Double)
  , lookingAt :: !(Point V3 Double)
  , viewUp :: !(Dir V3 Double)
  , thinLens :: !(Maybe ThinLens)
  }
  deriving (Show, Eq, Ord, Generic)

cmdP :: Opt.ParserInfo Options
cmdP = Opt.info (p <**> Opt.helper) $ Opt.progDesc "Renders spheres with diffusion"
  where
    p = do
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
                <> Opt.value 100
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
                <> Opt.value 400
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
        Opt.strOption $
          Opt.long "output"
            <> Opt.short 'o'
            <> Opt.metavar "PATH"
            <> Opt.help "Output path"
            <> Opt.showDefault
            <> Opt.value ("workspace" </> "camera.png")
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
                <> Opt.value (16 / 9)
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
                <> Opt.value 90.0
                <> Opt.help "Vertical field of view, in Degree"
                <> Opt.showDefault
            )
      cameraOrigin <-
        fmap p3 $
          Opt.option Opt.auto $
            Opt.long "camera-origin"
              <> Opt.short 'O'
              <> Opt.value (0, 0, 0)
              <> Opt.showDefault
              <> Opt.help "The camera origin where the camera looks from"
      lookingAt <-
        fmap p3 $
          Opt.option Opt.auto $
            Opt.long "look-at"
              <> Opt.short 'F'
              <> Opt.value (0, 0, -1)
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
      hollow <-
        Opt.flag' True (Opt.long "hollow" <> Opt.help "Makes glass ball hollow")
          <|> not <$> Opt.switch (Opt.long "no-hollow" <> Opt.help "Makes glass ball dense")
      mthinLens <- Opt.optional thinLensP
      pure $
        let thinLens = tieThinLens cameraOrigin lookingAt <$> mthinLens
         in Options {..}

data MThinLens = MThinLens {aperture :: !Double, focusDistance :: !(Maybe Double)}
  deriving (Show, Eq, Ord, Generic)

tieThinLens :: Point V3 Double -> Point V3 Double -> MThinLens -> ThinLens
tieThinLens src dst tl@MThinLens {aperture} =
  let focusDistance = fromMaybe (distance src dst) $ tl ^. #focusDistance
   in ThinLens {..}

thinLensP :: Opt.Parser MThinLens
thinLensP = do
  aperture <-
    Opt.option Opt.auto $
      Opt.long "aperture" <> Opt.short 'a' <> Opt.help "The aperture of a camera lens"
  focusDistance <-
    Opt.optional $
      Opt.option Opt.auto $
        Opt.long "focus-distance"
          <> Opt.long "fdist"
          <> Opt.short 'D'
          <> Opt.help "The focus distance of a camera lens. If --aperture is specified and this option is omitted, it will be set to the distance from the camera origin to looking-at point."
  pure MThinLens {..}

parseRatio :: String -> Maybe Rational
parseRatio =
  runReaderT $
    getAlt $
      foldMap
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

mkImage :: RandomGen g => g -> Options -> WordImage
mkImage g' opts@Options {..} =
  let imageHeight =
        floor $ fromIntegral imageWidth / aspectRatio
      scene = mkScene opts
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
        Random -> randomSamplingAntialias g' samplesPerPixel sz
        Stencil -> stencilAntialiasing g' (integerSquareRoot samplesPerPixel) sz
   in M.computeP $
        fromDoubleImage $
          correctGamma $
            antialias $
              curry $
                rayColour epsilon scene cutoff
                  <=< getRay aCamera . p2

p2 :: (a, a) -> Point V2 a
p2 = P . uncurry V2

mkScene :: Options -> Scene
mkScene Options {..} =
  let ground = Sphere {center = p3 (0, -100.5, -1), radius = 100}
      groundMaterial = Lambertian $ MkAttn 0.8 0.8 0.0
      centerMaterial = Lambertian $ MkAttn 0.1 0.2 0.5
      center = Sphere {center = p3 (0, 0, -1), radius = 0.5}
      leftMaterial = Dielectric 1.5
      leftS = Sphere {center = p3 (-1, 0, -1), radius = 0.5}
      hollowLeftSphere = Sphere {center = p3 (-1, 0, -1), radius = -0.45}
      rightRatio = MkAttn 0.8 0.6 0.2
      rightMaterial = Metal rightRatio
      rightS = Sphere {center = p3 (1, 0, -1), radius = 0.5}
      objs =
        [ mkSomeObject ground groundMaterial
        , mkSomeObject center centerMaterial
        , mkSomeObject leftS leftMaterial
        , mkSomeObject rightS rightMaterial
        ]
          ++ [ mkSomeObject hollowLeftSphere leftMaterial
             | hollow
             ]
      objects = fromObjectsWithBinBucket 16 4 objs
   in Scene
        { objects = objects
        , background = \Ray {..} ->
            let !unitDirection = normalize rayDirection
                !t = 0.5 * (unitDirection ^. _y + 1.0)
             in lerp t (PixelRGB 0.5 0.7 1.0) (PixelRGB 1.0 1.0 1.0)
        }

p3 :: (a, a, a) -> Point V3 a
p3 (x, y, z) = P $ V3 x y z
