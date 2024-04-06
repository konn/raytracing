{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeFamilies #-}

module System.Random.Stateful.STUnbox (
  STUGen (..),
  STUGenM (..),
  applySTUGen,
  runSTUGen,
  runSTUGen_,
) where

import Control.Monad.ST.Strict (RealWorld, ST, runST)
import Control.Monad.Trans.Maybe (MaybeT)
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as MU
import GHC.Generics
import RIO (NFData, PrimMonad (PrimState), Storable)
import System.Random.Orphans ()
import System.Random.Stateful

newtype STUGen g = STUGen {unSTUGen :: g}
  deriving (Show, Eq, Ord, Generic, Generic1, Functor, Foldable, Traversable)
  deriving newtype (RandomGen, NFData, Storable)

instance (PrimMonad m, U.Unbox g, RandomGen g) => FrozenGen (STUGen g) m where
  {-# SPECIALIZE instance FrozenGen (STUGen StdGen) IO #-}
  {-# SPECIALIZE instance FrozenGen (STUGen StdGen) (ST s) #-}
  {-# SPECIALIZE instance FrozenGen (STUGen StdGen) (MaybeT (ST s)) #-}
  {-# SPECIALIZE instance FrozenGen (STUGen StdGen) (MaybeT IO) #-}
  type MutableGen (STUGen g) m = STUGenM g (PrimState m)
  freezeGen = fmap STUGen . flip MU.unsafeRead 0 . unSTUGenM
  {-# INLINE freezeGen #-}
  thawGen = fmap STUGenM . U.unsafeThaw . U.singleton . unSTUGen
  {-# INLINE thawGen #-}

applySTUGen :: (PrimMonad m, U.Unbox g) => (g -> (a, g)) -> STUGenM g (PrimState m) -> m a
{-# INLINE applySTUGen #-}
{-# SPECIALIZE INLINE applySTUGen :: (StdGen -> (a, StdGen)) -> STUGenM StdGen s -> ST s a #-}
{-# SPECIALIZE INLINE applySTUGen :: (StdGen -> (a, StdGen)) -> STUGenM StdGen s -> MaybeT (ST s) a #-}
{-# SPECIALIZE INLINE applySTUGen :: (StdGen -> (a, StdGen)) -> STUGenM StdGen RealWorld -> IO a #-}
{-# SPECIALIZE INLINE applySTUGen :: (StdGen -> (a, StdGen)) -> STUGenM StdGen RealWorld -> MaybeT IO a #-}
applySTUGen f (STUGenM ref) = do
  g <- MU.unsafeRead ref 0
  case f g of
    (!a, !g') -> a <$ MU.unsafeWrite ref 0 g'

instance
  (PrimMonad m, RandomGen g, U.Unbox g, PrimState m ~ s) =>
  StatefulGen (STUGenM g s) m
  where
  {-# SPECIALIZE instance StatefulGen (STUGenM StdGen s) (ST s) #-}
  {-# SPECIALIZE instance StatefulGen (STUGenM StdGen s) (MaybeT (ST s)) #-}
  {-# SPECIALIZE instance StatefulGen (STUGenM StdGen RealWorld) IO #-}
  {-# SPECIALIZE instance StatefulGen (STUGenM StdGen RealWorld) (MaybeT IO) #-}
  uniformWord32R r = applySTUGen (genWord32R r)
  {-# INLINE uniformWord32R #-}
  uniformWord64R r = applySTUGen (genWord64R r)
  {-# INLINE uniformWord64R #-}
  uniformWord8 = applySTUGen genWord8
  {-# INLINE uniformWord8 #-}
  uniformWord16 = applySTUGen genWord16
  {-# INLINE uniformWord16 #-}
  uniformWord32 = applySTUGen genWord32
  {-# INLINE uniformWord32 #-}
  uniformWord64 = applySTUGen genWord64
  {-# INLINE uniformWord64 #-}
  uniformShortByteString n = applySTUGen (genShortByteString n)

instance
  (PrimState m ~ s, RandomGen g, U.Unbox g, PrimMonad m) =>
  RandomGenM (STUGenM g s) g m
  where
  {-# SPECIALIZE instance RandomGenM (STUGenM StdGen s) StdGen (ST s) #-}
  {-# SPECIALIZE instance RandomGenM (STUGenM StdGen s) StdGen (MaybeT (ST s)) #-}
  {-# SPECIALIZE instance RandomGenM (STUGenM StdGen RealWorld) StdGen IO #-}
  {-# SPECIALIZE instance RandomGenM (STUGenM StdGen RealWorld) StdGen (MaybeT IO) #-}
  applyRandomGenM = applySTUGen
  {-# INLINE applyRandomGenM #-}

newtype STUGenM g s = STUGenM {unSTUGenM :: U.MVector s g}

runSTUGen :: (RandomGen g, U.Unbox g) => g -> (forall s. STUGenM g s -> ST s a) -> (a, g)
{-# INLINE runSTUGen #-}
{-# SPECIALIZE INLINE runSTUGen :: StdGen -> (forall s. STUGenM StdGen s -> ST s a) -> (a, StdGen) #-}
runSTUGen g action = unSTUGen <$> runST (withMutableGen (STUGen g) action)

runSTUGen_ :: (RandomGen g, U.Unbox g) => g -> (forall s. STUGenM g s -> ST s a) -> a
{-# SPECIALIZE INLINE runSTUGen_ :: StdGen -> (forall s. STUGenM StdGen s -> ST s a) -> a #-}
{-# INLINE runSTUGen_ #-}
runSTUGen_ = fmap fst . runSTUGen
