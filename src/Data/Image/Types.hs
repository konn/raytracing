{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Image.Types (
  Pixel (..),
  DoubleImage,
  WordImage,
  width,
  height,
  fromDoubleImage,
  generateImage,
) where

import Control.Arrow ((>>>))
import Data.Distributive (Distributive (..))
import Data.Functor.Rep
import Data.Massiv.Array
import Data.Massiv.Array qualified as M
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as GM
import Data.Vector.Unboxed qualified as U
import Data.Word (Word8)
import GHC.Generics (Generic, Generic1)
import Linear (Additive, Metric)

data Pixel f = Pixel {red, green, blue :: !f}
  deriving (Show, Eq, Ord, Generic, Foldable, Functor, Traversable, Generic1)
  deriving anyclass (Representable, Additive, Metric)
  deriving (Applicative, Monad) via Co Pixel

instance Distributive Pixel where
  distribute = distributeRep
  {-# INLINE distribute #-}
  collect = collectRep
  {-# INLINE collect #-}

instance U.Unbox a => U.Unbox (Pixel a)

data instance U.Vector (Pixel f) = PVector !Int !(U.Vector f)

data instance U.MVector s (Pixel f) = PMVector !Int !(U.MVector s f)

instance U.Unbox f => G.Vector U.Vector (Pixel f) where
  basicUnsafeFreeze (PMVector n v) = PVector n <$> G.basicUnsafeFreeze v
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (PVector n v) = PMVector n <$> G.basicUnsafeThaw v
  {-# INLINE basicUnsafeThaw #-}
  basicLength (PVector n _) = n
  {-# INLINE basicLength #-}
  basicUnsafeSlice off len (PVector _ v) =
    PVector len (G.basicUnsafeSlice (3 * off) (3 * len) v)
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (PVector _ v) i = do
    let !j = i * 3
    Pixel
      <$> G.basicUnsafeIndexM v j
      <*> G.basicUnsafeIndexM v (j + 1)
      <*> G.basicUnsafeIndexM v (j + 2)
  {-# INLINE basicUnsafeIndexM #-}

instance U.Unbox f => GM.MVector U.MVector (Pixel f) where
  {-# INLINE basicLength #-}
  basicLength (PMVector n _) = n
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice off len (PMVector _ v) =
    PMVector len (GM.basicUnsafeSlice (3 * off) (3 * len) v)
  {-# INLINE basicOverlaps #-}
  basicOverlaps (PMVector _ l) (PMVector _ r) = GM.basicOverlaps l r
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeNew len = PMVector len <$> GM.basicUnsafeNew (3 * len)
  {-# INLINE basicInitialize #-}
  basicInitialize (PMVector _ v) = GM.basicInitialize v
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead (PMVector _ v) i = do
    let !j = 3 * i
    Pixel
      <$> GM.basicUnsafeRead v j
      <*> GM.basicUnsafeRead v (j + 1)
      <*> GM.basicUnsafeRead v (j + 2)
  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite (PMVector _ v) i (Pixel x y z) = do
    let !off = 3 * i
    GM.basicUnsafeWrite v off x
    GM.basicUnsafeWrite v (off + 1) y
    GM.basicUnsafeWrite v (off + 2) z

-- | Matrix of pixel, 0.0 to 1.0.
type DoubleImage = Matrix U (Pixel Double)

-- | Matrix of pixel, 0 to 255
type WordImage = Matrix U (Pixel Word8)

fromDoubleImage :: DoubleImage -> WordImage
fromDoubleImage = M.compute . M.map (fmap $ floor . (255.999 *))

width, height :: Size r => Matrix r f -> Int
width = M.size >>> \case (Sz2 _ w) -> w
height = M.size >>> \case (Sz2 h _) -> h

-- | almost same as makeArray, but places origin at the lower-left instead of upper-left.
generateImage :: Sz2 -> (Ix2 -> Pixel Double) -> WordImage
generateImage sz =
  fromDoubleImage
    . M.computeP
    . M.reverse M.Dim2
    . M.makeArray @M.D M.Par sz
