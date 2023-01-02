{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TupleSections #-}

module Data.Vector.Shuffle (shuffleM, shuffle) where

import Control.Monad.ST.Strict (ST, runST)
import Control.Monad.Trans.State.Strict (execStateT)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Generic.Mutable as MG
import qualified Data.Vector.Unboxed as U
import System.Random.Stateful

shuffleM :: (GM.MVector v a, RandomGen g) => v s a -> g -> ST s g
{-# SPECIALIZE INLINE shuffleM :: U.MVector s Int -> StdGen -> ST s StdGen #-}
{-# INLINE shuffleM #-}
shuffleM mv = execStateT (go (GM.length mv - 1))
  where
    go !i
      | i <= 0 = pure ()
      | otherwise = do
          r <- randomRM (0, i) StateGenM
          MG.unsafeSwap mv i r
          go $! i - 1

shuffle :: (G.Vector v a, RandomGen g) => v a -> g -> (v a, g)
{-# SPECIALIZE INLINE shuffle :: U.Vector Int -> StdGen -> (U.Vector Int, StdGen) #-}
{-# INLINE shuffle #-}
shuffle v g = runST $ do
  mv <- G.thaw v
  g' <- shuffleM mv g
  (,g') <$> G.unsafeFreeze mv
