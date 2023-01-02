{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Options (
  Options (..),
  Defaults,
  defaultOptions,
  Antialiasing (..),
  SceneName (..),
  sceneOptionsP,
) where

import Barbies (bzipWithC)
import Control.Applicative ((<|>))
import Control.Lens ((.~))
import Control.Monad (guard)
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.ByteString.Char8 qualified as BS
import Data.Char qualified as C
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Generic.HKD
import Data.Monoid (Alt (..), Last (..))
import Data.Scientific qualified as S
import Data.String (IsString)
import Data.Trie qualified as Trie
import GHC.Generics (Generic)
import Linear
import Linear.Affine
import Linear.Angle
import Linear.Direction
import Named
import Numeric.Natural (Natural)
import Options.Applicative (ReadM)
import Options.Applicative qualified as Opt
import RIO (Hashable)
import RIO.Text qualified as T
import RIO.Text.Partial qualified as T
import RayTracing.Camera (ThinLens (..))
import Text.Read (readMaybe)

data Antialiasing = Random | Stencil
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable)

defaultOptions :: (SceneName -> FilePath) -> SceneName -> HKD Options Last
defaultOptions toOut scene =
  mempty
    & field @"outputPath" .~ pure (toOut scene)
    & field @"cutoff" .~ pure 50
    & field @"samplesPerPixel" .~ pure 500
    & field @"imageWidth" .~ pure 1200
    & field @"epsilon" .~ pure 0.001
    & field @"antialiasing" .~ pure Random
    & field @"aspectRatio" .~ pure (3 / 2)
    & field @"verticalFieldOfView" .~ pure (20.0 @@ deg)
    & field @"cameraOrigin" .~ pure (p3 (13, 2, 3))
    & field @"lookingAt" .~ pure 0
    & field @"viewUp" .~ pure (dir $ V3 0 1 0)
    & field @"bucketSize" .~ pure 4
    & field @"binSize" .~ pure 128

data Options = Options
  { cutoff :: !Int
  , samplesPerPixel :: !Int
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
  , bucketSize :: !Int
  , binSize :: !Int
  , randomSeed :: !(Maybe Int)
  , scene :: !SceneName
  }
  deriving (Show, Eq, Ord, Generic)

data OptionP a where
  Option' ::
    !(a -> b) ->
    !(ReadM a) ->
    !String ->
    !(Opt.Mod Opt.OptionFields a) ->
    OptionP b
  RawP :: !(Opt.Parser a) -> OptionP a

instance Functor OptionP where
  fmap f (Option' g r desc m) = Option' (f . g) r desc m
  fmap f (RawP p) = RawP $ f <$> p
  {-# INLINE fmap #-}

type Defaults = HKD Options Last

sceneOptionsP :: SceneName -> (SceneName -> Defaults) -> Opt.Parser Options
sceneOptionsP scene defs =
  construct $
    bzipWithC @Show
      (setDefault . getLast)
      (defs scene)
      (optionDescription scene)

setDefault :: Show a => Maybe a -> OptionP a -> Opt.Parser a
setDefault Nothing (RawP p) = p
setDefault Nothing (Option' f reader desc m) =
  f <$> Opt.option reader (m <> Opt.help desc)
setDefault (Just v) (RawP p) = p <|> pure v
setDefault (Just v) (Option' f reader desc modif) =
  f
    <$> Opt.option
      reader
      (modif <> Opt.help (desc <> " (default: " <> show v <> ")"))
    <|> pure v

option :: ReadM a -> String -> Opt.Mod Opt.OptionFields a -> OptionP a
option = Option' id

optionAuto :: (Read a) => String -> Opt.Mod Opt.OptionFields a -> OptionP a
optionAuto = option Opt.auto

strOption :: (IsString str) => String -> Opt.Mod Opt.OptionFields str -> OptionP str
strOption = Option' id Opt.str

optionDescription :: SceneName -> HKD Options OptionP
optionDescription scene =
  record @Options
    ! #cutoff
      ( fromIntegral @Natural @Int
          <$> optionAuto
            "Maximum iteration to trace the diffusion"
            ( Opt.long "cutoff"
                <> Opt.short 'M'
                <> Opt.metavar "NUM"
            )
      )
    ! #samplesPerPixel
      ( fromIntegral @Natural
          <$> optionAuto
            "Sample size for randomised antialiasing"
            ( Opt.long "samples"
                <> Opt.short 's'
                <> Opt.metavar "NUM"
            )
      )
    ! #imageWidth
      ( fromIntegral @Natural
          <$> optionAuto
            "Sample size for randomised antialiasing"
            ( Opt.long "width"
                <> Opt.short 'w'
                <> Opt.metavar "NUM"
            )
      )
    ! #epsilon
      ( option
          (Opt.auto @Double >>= \d -> d <$ guard (d >= 0))
          "The threshold to regard as zero"
          $ Opt.long "epsilon"
            <> Opt.short 'E'
      )
    ! #outputPath
      ( strOption "Output path" $
          Opt.long "output"
            <> Opt.short 'o'
            <> Opt.metavar "PATH"
      )
    ! #antialiasing
      ( option (Opt.maybeReader parseAntialising) "Antialiasing method" $
          Opt.long "antialias"
            <> Opt.short 'A'
      )
    ! #aspectRatio
      ( fromRational
          <$> option
            (Opt.maybeReader parseRatio)
            "Aspect ratio"
            (Opt.long "aspect")
      )
    ! #verticalFieldOfView
      ( (@@ deg)
          <$> optionAuto
            "Vertical field of view, in Degree"
            ( Opt.long "vfov"
                <> Opt.short 'V'
                <> Opt.long "vertical-fov"
            )
      )
    ! #cameraOrigin
      ( p3
          <$> optionAuto
            "The camera origin where the camera looks from"
            (Opt.long "camera-origin" <> Opt.short 'O')
      )
    ! #lookingAt
      ( p3
          <$> optionAuto
            "The point that the camera is looking at"
            (Opt.long "look-at" <> Opt.short 'F')
      )
    ! #viewUp
      ( optionAuto
          "The View-Up vector of the camera"
          (Opt.long "view-up" <> Opt.long "vup" <> Opt.short 'U')
          <&> \(x, y, z) -> dir (V3 x y z)
      )
    ! #thinLens
      ( RawP $
          Opt.flag' Nothing (Opt.long "no-lens" <> Opt.help "renders without a lens")
            <|> Just <$> thinLensP
      )
    ! #randomSeed
      ( RawP $
          Opt.optional $
            Opt.option Opt.auto $
              Opt.long "seed"
                <> Opt.short 'S'
                <> Opt.help "If specified, use the number as the seed for the random number"
      )
    ! #bucketSize
      ( optionAuto "The maximum # of objects to store in a BVH Node" $
          Opt.long "bucket-size" <> Opt.short 'B'
      )
    ! #binSize
      ( optionAuto "The bin size used for binning in BVH construction" $
          Opt.long "bin-size" <> Opt.short 'b'
      )
    ! #scene (RawP $ pure scene)

data SceneName = RandomScene | TwoSpheres | RayCharles | Earth | Perlin
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable)

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

parseRatio :: String -> Maybe Rational
parseRatio =
  runReaderT $
    getAlt $
      foldMap @[]
        (Alt . ReaderT)
        [ readMaybe . T.unpack . T.replace "/" "%" . T.pack
        , fmap (toRational . S.toRealFloat @Double) . readMaybe
        ]

p3 :: (a, a, a) -> Point V3 a
p3 (x, y, z) = P $ V3 x y z

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