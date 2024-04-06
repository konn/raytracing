{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Avg (Avg (..), getAvg) where

import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as MG
import Data.Vector.Unboxed qualified as U
import GHC.Generics (Generic)

data Avg a = Avg {-# UNPACK #-} !Word !a
  deriving (Show, Eq, Ord, Generic)

instance U.IsoUnbox (Avg a) (Word, a)

newtype instance U.Vector (Avg a) = MV_Avg (U.Vector (Word, a))

newtype instance U.MVector s (Avg a) = MU_Avg (U.MVector s (Word, a))

deriving via
  Avg a `U.As` (Word, a)
  instance
    (U.Unbox a) => G.Vector U.Vector (Avg a)

deriving via
  Avg a `U.As` (Word, a)
  instance
    (U.Unbox a) =>
    MG.MVector U.MVector (Avg a)

instance (U.Unbox a) => U.Unbox (Avg a)

instance (Num a) => Semigroup (Avg a) where
  Avg cntl suml <> Avg cntr sumr = Avg (cntl + cntr) (suml + sumr)
  {-# INLINE (<>) #-}

instance (Num a) => Monoid (Avg a) where
  mempty = Avg 0 0
  {-# INLINE mempty #-}

getAvg :: (Floating a) => Avg a -> a
{-# INLINE getAvg #-}
getAvg (Avg cnt total) = total / fromIntegral cnt
