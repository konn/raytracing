{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Control.Applicative ((<**>))
import Control.Arrow ((<<<))
import Control.Lens
import Control.Monad (guard)
import Data.Avg
import Data.Generics.Labels ()
import Data.Image.Format.PPM
import Data.Image.Types
import Data.Massiv.Array (Sz (..))
import Data.Massiv.Array qualified as M
import GHC.Generics (Generic)
import Linear
import Linear.Affine (Point (..))
import Numeric.Natural (Natural)
import Options.Applicative qualified as Opt
import RIO.FilePath ((</>))
import RayTracing.Camera
import RayTracing.Object
import RayTracing.Object.Sphere
import RayTracing.Ray
import System.Random
import System.Random.Stateful (randomRM, runSTGen)

main :: IO ()
main = do
  opts@Options {..} <- Opt.execParser cmdP
  g <- getStdGen
  writePPMFile outputPath $ mkImage g opts

data Diffusion = Lambert | Hemisphere
  deriving (Show, Eq, Ord, Generic)

data Options = Options
  { cutoff :: !Int
  , samplesPerPixel :: !Int
  , imageWidth :: !Int
  , epsilon :: !Double
  , outputPath :: !FilePath
  , fuzzy :: !Bool
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
            <> Opt.value ("workspace" </> "metal-spheres.ppm")
      fuzzy <-
        Opt.switch $
          Opt.long "fuzzy"
            <> Opt.short 'F'
            <> Opt.showDefault
            <> Opt.help "Enable fuzzy metal rendering"
      pure Options {..}

aCamera :: Camera
aCamera = mkCamera defaultCameraConfig

mkImage :: RandomGen g => g -> Options -> WordImage
mkImage g0 opts@Options {..} =
  let imageHeight =
        floor $
          fromIntegral imageWidth / defaultCameraConfig ^. #aspectRatio
      scene = mkScene opts
   in fromDoubleImage $
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
                            !dx <- randomRM (-1.0, 1.0) g
                            !dy <- randomRM (-1.0, 1.0) g
                            let !u = (fromIntegral i + dx) / (fromIntegral imageWidth - 1)
                                !v = (fromIntegral j + dy) / (fromIntegral imageHeight - 1)
                                !r = getRay aCamera $ P $ V2 u v
                            Avg 1 <$> rayColour epsilon scene g cutoff r
                      )

mkScene :: Options -> Scene
mkScene Options {..} =
  let ground = Sphere {center = p3 (0, -100.5, -1), radius = 100}
      groundMaterial = Lambertian $ MkAttn 0.8 0.8 0.0
      centerMaterial = Lambertian $ MkAttn 0.7 0.3 0.3
      center = Sphere {center = p3 (0, 0, -1), radius = 0.5}
      leftRatio = MkAttn 0.8 0.8 0.8
      leftMaterial
        | fuzzy = MkSomeMaterial $ FuzzyMetal leftRatio 0.3
        | otherwise = MkSomeMaterial $ Metal leftRatio
      leftS = Sphere {center = p3 (-1, 0, -1), radius = 0.5}
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
            , MkSomeObject rightS rightMaterial
            ]
        , background = \Ray {..} ->
            let !unitDirection = normalize rayDirection
                !t = 0.5 * (unitDirection ^. _y + 1.0)
             in lerp t (Pixel 0.5 0.7 1.0) (Pixel 1.0 1.0 1.0)
        }

p3 :: (a, a, a) -> Point V3 a
p3 (x, y, z) = P $ V3 x y z
