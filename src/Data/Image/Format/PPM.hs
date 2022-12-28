{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Image.Format.PPM (formatPPM, writePPMFile, exampleGradient) where

import qualified Data.ByteString.Builder as BB
import Data.Image.Types
import Data.Massiv.Array (Ix2 (..))
import qualified Data.Massiv.Array as M
import Data.Word
import RIO (IOMode (..), MonadUnliftIO, liftIO, withFile)

writePPMFile :: MonadUnliftIO m => FilePath -> WordImage -> m ()
writePPMFile fp img = withFile fp WriteMode $ \h ->
  liftIO $ BB.hPutBuilder h $ formatPPM img

formatPPM :: WordImage -> BB.Builder
formatPPM img = formatPPMHeader img <> M.foldMono formatPixel img

formatPPMHeader :: WordImage -> BB.Builder
formatPPMHeader img =
  let M.Sz2 !h !w = M.size img
   in "P3\n" <> BB.intDec w <> " " <> BB.intDec h <> "\n255\n"

formatPixel :: Pixel RGB Word8 -> BB.Builder
formatPixel (PixelRGB red green blue) =
  BB.word8Dec red <> " " <> BB.word8Dec green <> " " <> BB.word8Dec blue <> "\n"

exampleGradient :: WordImage
exampleGradient = generateImage (M.Sz2 256 256) $ \(j :. i) ->
  PixelRGB (fromIntegral i / 255) (fromIntegral j / 255) 0.25

-- >>> writePPMFile "workspace/grad.ppm" exampleGradient
