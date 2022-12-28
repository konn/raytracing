{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Control.Applicative ((<**>))
import Control.Lens
import Control.Monad (guard, (<=<))
import Data.ByteString.Char8 qualified as BS
import Data.Char qualified as C
import Data.Generics.Labels ()
import Data.Image.Antialiasing (randomSamplingAntialias, stencilAntialiasing)
import Data.Image.Format.PPM
import Data.Image.Types
import Data.Massiv.Array (Sz (..))
import Data.Massiv.Array qualified as M
import Data.Trie qualified as Trie
import GHC.Generics (Generic)
import Linear
import Linear.Affine (Point (..))
import Math.NumberTheory.Roots (integerSquareRoot)
import Numeric.Natural (Natural)
import Options.Applicative qualified as Opt
import RIO.FilePath ((</>))
import RayTracing.Camera
import RayTracing.Object
import RayTracing.Object.Sphere
import RayTracing.Ray
import System.Random

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
  , fuzzy :: !Bool
  , antialiasing :: !Antialiasing
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
            <> Opt.value ("workspace" </> "glasses.ppm")
      fuzzy <-
        Opt.switch $
          Opt.long "fuzzy"
            <> Opt.short 'F'
            <> Opt.showDefault
            <> Opt.help "Enable fuzzy metal rendering"
      antialiasing <-
        Opt.option (Opt.maybeReader parseAntialising) $
          Opt.long "antialias"
            <> Opt.short 'A'
            <> Opt.value Random
            <> Opt.showDefault
            <> Opt.help "Antialiasing method"
      pure Options {..}

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

aCamera :: Camera
aCamera = mkCamera defaultCameraConfig

mkImage :: RandomGen g => g -> Options -> WordImage
mkImage g0 opts@Options {..} =
  let imageHeight =
        floor $
          fromIntegral imageWidth / defaultCameraConfig ^. #aspectRatio
      scene = mkScene opts
      sz = Sz2 imageHeight imageWidth
      antialias = case antialiasing of
        Random -> randomSamplingAntialias g0 samplesPerPixel sz
        Stencil -> stencilAntialiasing g0 (integerSquareRoot samplesPerPixel) sz
   in M.computeP $
        fromDoubleImage $
          correctGamma $
            antialias $ \g ->
              curry $
                rayColour epsilon scene g cutoff
                  <=< getRay g aCamera . p2

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
      hollowLeftSphere = Sphere {center = p3 (-1, 0, -1), radius = -0.4}
      rightRatio = MkAttn 0.8 0.6 0.2
      rightMaterial
        | fuzzy = MkSomeMaterial $ FuzzyMetal rightRatio 1.0
        | otherwise = MkSomeMaterial $ Metal rightRatio
      rightS = Sphere {center = p3 (1, 0, -1), radius = 0.5}
   in Scene
        { objects =
            [ MkSomeObject ground groundMaterial
            , MkSomeObject center centerMaterial
            , MkSomeObject leftS leftMaterial
            , MkSomeObject hollowLeftSphere leftMaterial
            , MkSomeObject rightS rightMaterial
            ]
        , background = \Ray {..} ->
            let !unitDirection = normalize rayDirection
                !t = 0.5 * (unitDirection ^. _y + 1.0)
             in lerp t (PixelRGB 0.5 0.7 1.0) (PixelRGB 1.0 1.0 1.0)
        }

p3 :: (a, a, a) -> Point V3 a
p3 (x, y, z) = P $ V3 x y z
