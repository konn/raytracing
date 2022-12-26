{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Control.Applicative ((<**>))
import Control.Lens
import Control.Monad (guard)
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.ByteString.Char8 qualified as BS
import Data.Char qualified as C
import Data.Generics.Labels ()
import Data.Image.Antialiasing (randomSamplingAntialias, stencilAntialiasing)
import Data.Image.Format.PPM
import Data.Image.Types
import Data.Massiv.Array (Sz (..))
import Data.Massiv.Array qualified as M
import Data.Monoid (Alt (..))
import Data.Scientific qualified as S
import Data.Trie qualified as Trie
import GHC.Generics (Generic)
import Linear
import Linear.Affine (Point (..))
import Linear.Angle (Angle, deg, (@@))
import Math.NumberTheory.Roots (integerSquareRoot)
import Numeric.Natural (Natural)
import Options.Applicative qualified as Opt
import RIO.FilePath ((</>))
import RIO.Text qualified as T
import RIO.Text.Partial qualified as T
import RayTracing.Camera
import RayTracing.Object
import RayTracing.Object.Sphere
import RayTracing.Ray
import System.Random
import Text.Read (readMaybe)

main :: IO ()
main = do
  opts@Options {..} <- Opt.execParser cmdP
  g <- getStdGen
  writePPMFile outputPath $ mkImage g opts

data Diffusion = Lambert | Hemisphere
  deriving (Show, Eq, Ord, Generic)

data Antialiasing = Random | Stencil
  deriving (Show, Eq, Ord, Generic)

data Options = Options
  { cutoff :: !Int
  , samplesPerPixel :: !Int
  , imageWidth :: !Int
  , epsilon :: !Double
  , outputPath :: !FilePath
  , antialiasing :: !Antialiasing
  , aspectRatio :: !Double
  , verticalFieldOfView :: !(Angle Double)
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
            <> Opt.value ("workspace" </> "camera.ppm")
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
      pure Options {..}

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
mkImage g0 opts@Options {..} =
  let imageHeight =
        floor $
          fromIntegral imageWidth / defaultCameraConfig ^. #aspectRatio
      scene = mkScene opts
      sz = Sz2 imageHeight imageWidth
      aCamera =
        mkCamera $
          defaultCameraConfig
            & #aspectRatio .~ aspectRatio
            & #verticalFieldOfView .~ verticalFieldOfView
      antialias = case antialiasing of
        Random -> randomSamplingAntialias g0 samplesPerPixel sz
        Stencil -> stencilAntialiasing g0 (integerSquareRoot samplesPerPixel) sz
   in fromDoubleImage $
        M.computeP $
          correctGamma $
            antialias $
              \g ->
                curry $ rayColour epsilon scene g cutoff . getRay aCamera . p2

p2 :: (a, a) -> Point V2 a
p2 = P . uncurry V2

mkScene :: Options -> Scene
mkScene Options {} =
  let r = cos $ pi / 4
      leftMaterial = Lambertian $ MkAttn 0 0 1
      leftS = Sphere (p3 (-r, 0, -1)) r
      rightS = Sphere (p3 (r, 0, -1)) r
      rightMaterial = Lambertian $ MkAttn 1 0 0
   in Scene
        { objects =
            [ MkSomeObject leftS leftMaterial
            , MkSomeObject rightS rightMaterial
            ]
        , background = \Ray {..} ->
            let !unitDirection = normalize rayDirection
                !t = 0.5 * (unitDirection ^. _y + 1.0)
             in lerp t (Pixel 0.5 0.7 1.0) (Pixel 1.0 1.0 1.0)
        }

p3 :: (a, a, a) -> Point V3 a
p3 (x, y, z) = P $ V3 x y z
