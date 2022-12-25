{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Avg (Avg (..), getAvg) where

import Data.Image.Types
import Data.Vector.Unboxed.Deriving (derivingUnbox)
import Linear

data Avg = Avg !Word !(Pixel Double)
  deriving (Show, Eq, Ord)

derivingUnbox
  "Avg"
  [t|Avg -> (Word, Pixel Double)|]
  [|\(Avg c s) -> (c, s)|]
  [|uncurry Avg|]

instance Semigroup Avg where
  Avg cntl suml <> Avg cntr sumr = Avg (cntl + cntr) (suml ^+^ sumr)
  {-# INLINE (<>) #-}

instance Monoid Avg where
  mempty = Avg 0 $ Pixel 0 0 0
  {-# INLINE mempty #-}

getAvg :: Avg -> Pixel Double
getAvg (Avg cnt total) = total ^/ fromIntegral cnt
