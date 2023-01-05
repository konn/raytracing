{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module System.Random.Orphans () where

import Control.Monad ((<$!>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Writer.CPS (WriterT)
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as MG
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Deriving (derivingUnbox)
import Data.Word (Word64)
import System.Random.Internal
import System.Random.SplitMix
import System.Random.Stateful

instance {-# INCOHERENT #-} StatefulGen g m => StatefulGen g (MaybeT m) where
  uniformWord64 = lift . uniformWord64
  {-# INLINE uniformWord64 #-}
  uniformWord32 = lift . uniformWord32
  {-# INLINE uniformWord32 #-}
  uniformWord32R = fmap lift . uniformWord32R
  {-# INLINE uniformWord32R #-}
  uniformWord64R = fmap lift . uniformWord64R
  {-# INLINE uniformWord64R #-}
  uniformWord8 = lift . uniformWord8
  {-# INLINE uniformWord8 #-}
  uniformWord16 = lift . uniformWord16
  {-# INLINE uniformWord16 #-}
  uniformShortByteString = fmap lift . uniformShortByteString
  {-# INLINE uniformShortByteString #-}

instance {-# INCOHERENT #-} RandomGenM g r m => RandomGenM g r (MaybeT m) where
  applyRandomGenM = fmap lift . applyRandomGenM
  {-# INLINE applyRandomGenM #-}

instance {-# INCOHERENT #-} (StatefulGen g m) => StatefulGen g (WriterT w m) where
  uniformWord64 = lift . uniformWord64
  {-# INLINE uniformWord64 #-}
  uniformWord32 = lift . uniformWord32
  {-# INLINE uniformWord32 #-}
  uniformWord32R = fmap lift . uniformWord32R
  {-# INLINE uniformWord32R #-}
  uniformWord64R = fmap lift . uniformWord64R
  {-# INLINE uniformWord64R #-}
  uniformWord8 = lift . uniformWord8
  {-# INLINE uniformWord8 #-}
  uniformWord16 = lift . uniformWord16
  {-# INLINE uniformWord16 #-}
  uniformShortByteString = fmap lift . uniformShortByteString
  {-# INLINE uniformShortByteString #-}

instance {-# OVERLAPPING #-} (RandomGenM g r m) => RandomGenM g r (WriterT w m) where
  applyRandomGenM = fmap lift . applyRandomGenM
  {-# INLINE applyRandomGenM #-}

data instance U.Vector SMGen = V_SMGen !Int (U.Vector Word64)

data instance U.MVector s SMGen = MV_SMGen !Int (U.MVector s Word64)

instance G.Vector U.Vector SMGen where
  basicUnsafeFreeze = \case
    MV_SMGen len mu -> V_SMGen len <$!> G.basicUnsafeFreeze mu
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw = \case
    V_SMGen len u -> MV_SMGen len <$!> G.basicUnsafeThaw u
  {-# INLINE basicUnsafeThaw #-}
  basicLength = \case
    V_SMGen len _ -> len
  {-# INLINE basicLength #-}
  basicUnsafeSlice off count (V_SMGen _ u) =
    V_SMGen count $ G.basicUnsafeSlice (2 * off) (2 * count) u
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_SMGen _ u) i =
    let !n = 2 * i
     in seedSMGen <$!> G.basicUnsafeIndexM u n <*> G.basicUnsafeIndexM u n
  {-# INLINE basicUnsafeIndexM #-}

instance MG.MVector U.MVector SMGen where
  basicLength = \case
    MV_SMGen len _ -> len
  {-# INLINE basicLength #-}
  basicUnsafeSlice off count (MV_SMGen _ u) =
    MV_SMGen count $ MG.basicUnsafeSlice (2 * off) (2 * count) u
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps (MV_SMGen _ u) (MV_SMGen _ v) = MG.basicOverlaps u v
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew n = MV_SMGen n <$> MG.basicUnsafeNew (2 * n)
  {-# INLINE basicUnsafeNew #-}
  basicInitialize (MV_SMGen _ u) = MG.basicInitialize u
  {-# INLINE basicInitialize #-}
  basicUnsafeRead (MV_SMGen _ mu) i = do
    let !off = 2 * i
    seedSMGen <$!> MG.basicUnsafeRead mu off <*> MG.basicUnsafeRead mu (off + 1)
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_SMGen _ mu) i gen = do
    let !off = 2 * i
        (!l, !r) = unseedSMGen gen
    MG.basicUnsafeWrite mu off l
    MG.basicUnsafeWrite mu (off + 1) r
  {-# INLINE basicUnsafeWrite #-}

derivingUnbox
  "StdGen"
  [t|StdGen -> SMGen|]
  [|\(StdGen sg) -> sg|]
  [|StdGen|]
