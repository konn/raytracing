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
import Data.Char qualified as C
import Data.Foldable (asum)
import Data.Generics.Labels ()
import Data.Image.Format.PPM
import Data.Image.Types
import Data.List (inits)
import Data.Massiv.Array (Sz (..))
import Data.Massiv.Array qualified as M
import GHC.Generics (Generic)
import Linear
import Linear.Affine (Point (..), (.+^), (.-.))
import Linear.Direction
import Numeric.Natural (Natural)
import Options.Applicative qualified as Opt
import RIO (ReaderT (..))
import RIO.FilePath ((</>))
import RayTracing.Camera
import RayTracing.Object.Classes
import RayTracing.Object.Sphere
import RayTracing.Ray
import System.Random
import System.Random.Stateful (RandomGenM (..), randomRM, runSTGen)
import System.Random.Utils (randomPointOnUnitHemisphere, randomPointOnUnitSphere)

main :: IO ()
main = do
  opts <- Opt.execParser cmdP
  g <- getStdGen
  writePPMFile ("workspace" </> "diffuse.ppm") $ mkImage g opts

data Diffusion = Lambert | Hemisphere
  deriving (Show, Eq, Ord, Generic)

data Options = Options
  { diffusion :: !Diffusion
  , cutoff :: !Int
  , samplesPerPixel :: !Int
  , imageWidth :: !Int
  }
  deriving (Show, Eq, Ord, Generic)

cmdP :: Opt.ParserInfo Options
cmdP = Opt.info (p <**> Opt.helper) $ Opt.progDesc "Renders spheres with diffusion"
  where
    p = do
      diffusion <-
        Opt.option (Opt.maybeReader parseDiffusion) $
          Opt.long "diffusion"
            <> Opt.short 'd'
            <> Opt.metavar "METHOD"
            <> Opt.value Lambert
            <> Opt.help "Diffusion method (lambert, or hemisphere)"
            <> Opt.showDefault
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

      pure Options {..}

parseDiffusion :: String -> Maybe Diffusion
parseDiffusion =
  runReaderT $
    asum
      [ ReaderT $ \txt -> do method <$ guard (map C.toLower txt == label)
      | method <- [Lambert, Hemisphere]
      , label <- drop 1 . inits . map C.toLower $ show method
      ]

aCamera :: Camera
aCamera = mkCamera defaultCameraConfig

mkImage :: RandomGen g => g -> Options -> WordImage
mkImage g0 opts@Options {..} =
  let imageHeight =
        floor $
          fromIntegral imageWidth
            / defaultCameraConfig
            ^. #aspectRatio
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
                            !c <- randomRM (0, 1.0) g
                            let !u = (fromIntegral i + c) / (fromIntegral imageWidth - 1)
                                !v = (fromIntegral j + c) / (fromIntegral imageHeight - 1)
                                !r = getRay aCamera $ P $ V2 u v
                            Avg 1 <$> colorRayDiffuse opts world g cutoff r
                      )

epsilon :: Double
epsilon = 0.01

colorRayDiffuse :: (RandomGenM g r m, Hittable obj) => Options -> obj -> g -> Int -> Ray -> m (Pixel Double)
colorRayDiffuse Options {..} obj g = go
  where
    {-# INLINE go #-}
    go !depth r@Ray {..}
      | depth <= 0 = pure $ Pixel 0 0 0
      | Just Hit {..} <- hitWithin (Just epsilon) Nothing r obj = do
          let n = unDir normal
          dev <- applyRandomGenM (randomDiffusion diffusion normal) g
          let target = coord .+^ n .+^ unDir dev
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

randomDiffusion :: RandomGen g => Diffusion -> Dir V3 Double -> g -> (Dir V3 Double, g)
randomDiffusion Lambert = const randomPointOnUnitSphere
randomDiffusion Hemisphere = randomPointOnUnitHemisphere

world :: [Sphere]
world = [sphere1, sphere2]

p3 :: (a, a, a) -> Point V3 a
p3 (x, y, z) = P $ V3 x y z

sphere1, sphere2 :: Sphere
sphere1 = Sphere {center = p3 (0, 0, -1), radius = 0.5}
sphere2 = Sphere {center = p3 (0, -100.5, -1), radius = 100}
