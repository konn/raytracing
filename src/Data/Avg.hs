{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Avg (Avg (..), getAvg) where

import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Deriving (derivingUnbox)

data Avg a = Avg {-# UNPACK #-} !Word !a
  deriving (Show, Eq, Ord)

derivingUnbox
  "Avg"
  [t|forall a. U.Unbox a => Avg a -> (Word, a)|]
  [|\(Avg c s) -> (c, s)|]
  [|uncurry Avg|]

instance Num a => Semigroup (Avg a) where
  Avg cntl suml <> Avg cntr sumr = Avg (cntl + cntr) (suml + sumr)
  {-# INLINE (<>) #-}

instance Num a => Monoid (Avg a) where
  mempty = Avg 0 0
  {-# INLINE mempty #-}

getAvg :: Floating a => Avg a -> a
{-# INLINE getAvg #-}
getAvg (Avg cnt total) = total / fromIntegral cnt
