{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module System.Random.Orphans () where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Writer.CPS (WriterT)
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
