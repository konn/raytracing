{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GHC2021 #-}

module System.Random.Effectful (StaticLocalStateGenM (..)) where

import Effectful
import Effectful.NonDet (NonDet)
import Effectful.State.Static.Local
import GHC.Generics
import System.Random.Stateful

data StaticLocalStateGenM g = StaticLocalStateGenM
  deriving (Show, Eq, Ord, Generic, Generic1)

instance
  (RandomGen g, State g :> es) =>
  StatefulGen (StaticLocalStateGenM g) (Eff es)
  where
  {-# SPECIALIZE instance StatefulGen (StaticLocalStateGenM StdGen) (Eff '[State StdGen]) #-}
  {-# SPECIALIZE instance StatefulGen (StaticLocalStateGenM StdGen) (Eff '[NonDet, State StdGen]) #-}
  {-# SPECIALIZE instance
    RandomGen g =>
    StatefulGen (StaticLocalStateGenM g) (Eff '[State g])
    #-}
  {-# SPECIALIZE instance
    RandomGen g =>
    StatefulGen (StaticLocalStateGenM g) (Eff '[NonDet, State g])
    #-}
  uniformWord32R r = const $ state @g (genWord32R r)
  {-# INLINE uniformWord32R #-}
  uniformWord64R r = const $ state @g (genWord64R r)
  {-# INLINE uniformWord64R #-}
  uniformWord8 = const $ state @g genWord8
  {-# INLINE uniformWord8 #-}
  uniformWord16 = const $ state @g genWord16
  {-# INLINE uniformWord16 #-}
  uniformWord32 = const $ state @g genWord32
  {-# INLINE uniformWord32 #-}
  uniformWord64 = const $ state @g genWord64
  {-# INLINE uniformWord64 #-}
  uniformShortByteString n = const $ state @g (genShortByteString n)

instance
  (RandomGen g, State g :> es) =>
  RandomGenM (StaticLocalStateGenM g) g (Eff es)
  where
  {-# SPECIALIZE instance
    RandomGenM
      (StaticLocalStateGenM StdGen)
      StdGen
      (Eff '[State StdGen])
    #-}
  {-# SPECIALIZE instance
    RandomGenM
      (StaticLocalStateGenM StdGen)
      StdGen
      (Eff '[NonDet, State StdGen])
    #-}
  {-# SPECIALIZE instance
    RandomGen g =>
    RandomGenM
      (StaticLocalStateGenM g)
      g
      (Eff '[State g])
    #-}
  {-# SPECIALIZE instance
    RandomGen g =>
    RandomGenM
      (StaticLocalStateGenM g)
      g
      (Eff '[NonDet, State g])
    #-}
  applyRandomGenM = const . state @g
  {-# INLINE applyRandomGenM #-}
