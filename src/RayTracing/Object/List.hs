{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module RayTracing.Object.List (
  SomeHittable (..),
  SomeHittables (..),
  FoldHittables (..),
  singleton,
) where

import Data.FMList (FMList)
import Data.FMList qualified as FML
import GHC.Exts (IsList (..))
import GHC.Generics (Generic)
import RayTracing.Object.Classes

newtype SomeHittables = SomeHittables {objects :: FMList SomeHittable}
  deriving (Generic)
  deriving newtype (Semigroup, Monoid)
  deriving (Hittable) via FoldHittables FMList SomeHittable

instance IsList SomeHittables where
  type Item SomeHittables = SomeHittable
  fromList = SomeHittables . FML.fromList
  {-# INLINE fromList #-}
  toList = FML.toList . objects

singleton :: Hittable obj => obj -> SomeHittables
{-# INLINE singleton #-}
singleton = SomeHittables . FML.singleton . MkSomeHittable
