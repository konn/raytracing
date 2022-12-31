{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Data.Vector.Shuffle (shuffleM, shuffle) where

import Control.Monad.ST.Strict (ST)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Generic.Mutable as MG
import System.Random.Stateful

shuffleM :: (GM.MVector v a, RandomGen g) => v s a -> STGenM g s -> ST s ()
shuffleM mv g = go (GM.length mv - 1)
  where
    go !i
      | i <= 0 = pure ()
      | otherwise = do
          r <- randomRM (0, i) g
          MG.unsafeSwap mv i r
          go $! i - 1

shuffle :: (G.Vector v a, RandomGen g) => v a -> g -> (v a, g)
shuffle v g = runSTGen g $ \mg -> do
  mv <- G.thaw v
  shuffleM mv mg
  G.unsafeFreeze mv
