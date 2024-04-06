{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module System.Random.Orphans () where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Writer.CPS (WriterT)
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as MG
import Data.Vector.Unboxed qualified as U
import Data.Word (Word64)
import Linear.V2 (V2 (..))
import System.Random.Internal
import System.Random.SplitMix
import System.Random.Stateful

instance {-# INCOHERENT #-} (StatefulGen g m) => StatefulGen g (MaybeT m) where
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

instance {-# INCOHERENT #-} (RandomGenM g r m) => RandomGenM g r (MaybeT m) where
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

newtype instance U.Vector SMGen = V_SMGen (U.Vector (V2 Word64))

newtype instance U.MVector s SMGen = MV_SMGen (U.MVector s (V2 Word64))

deriving instance U.Unbox SMGen

deriving via SMGen `U.As` V2 Word64 instance G.Vector U.Vector SMGen

deriving via SMGen `U.As` V2 Word64 instance MG.MVector U.MVector SMGen

instance U.IsoUnbox SMGen (V2 Word64) where
  toURepr = uncurry V2 . unseedSMGen
  {-# INLINE toURepr #-}
  fromURepr (V2 l r) = seedSMGen l r
  {-# INLINE fromURepr #-}

newtype instance U.Vector StdGen = V_StdGen (U.Vector SMGen)

newtype instance U.MVector s StdGen = MV_StdGen (U.MVector s SMGen)

deriving newtype instance G.Vector U.Vector StdGen

deriving newtype instance MG.MVector U.MVector StdGen

deriving anyclass instance U.Unbox StdGen
