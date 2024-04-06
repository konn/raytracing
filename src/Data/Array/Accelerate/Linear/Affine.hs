{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Array.Accelerate.Linear.Affine (
  Point (..),
  Affine (..),
  pattern P_,
  unP_,
  origin,
  _Point',
) where

import Control.Lens (Each (..), Iso, iso)
import Data.Array.Accelerate
import Data.Array.Accelerate qualified as A
import Data.Array.Accelerate.Data.Functor qualified as A
import Data.Array.Accelerate.Linear
import Data.Array.Accelerate.Smart (Exp (..))
import Data.Function (on)
import Data.Functor.Product (Product)
import Data.Type.Equality
import GHC.Float qualified as P
import Linear.Affine (Diff)
import Linear.Affine hiding (Affine (..), origin)
import Linear.Affine qualified as L
import Prelude qualified as P

deriving newtype instance (Elt (v a)) => Elt (Point v a)

pattern P_ :: Exp (v a) -> Exp (Point v a)
pattern P_ v <- Exp (Exp -> v)
  where
    P_ (Exp v) = Exp v

{-# COMPLETE P_ #-}

unP_ :: Exp (Point v a) -> Exp (v a)
unP_ (Exp v) = Exp v

instance (Lift Exp (v a), Plain (v a) ~ v (Plain a)) => Lift Exp (Point v a) where
  type Plain (Point v a) = v (Plain a)
  lift = lift . unP
  {-# INLINE lift #-}

instance
  (Unlift Exp (v a), Plain (v a) ~ v (Plain a)) =>
  Unlift Exp (Point v a)
  where
  unlift = P . unlift
  {-# INLINE unlift #-}

instance (Additive v) => Additive (Point v)

instance (Metric v) => Metric (Point v)

instance (R1 v) => R1 (Point v)

instance (R2 v) => R2 (Point v)

instance (R3 v) => R3 (Point v)

instance (R4 v) => R4 (Point v)

instance
  (Each (Exp (v a)) (Exp (v b)) (Exp a) (Exp b)) =>
  Each (Exp (Point v a)) (Exp (Point v b)) (Exp a) (Exp b)
  where
  each =
    _Point' @v @a @b . each @(Exp (v a)) @(Exp (v b)) @(Exp a) @(Exp b)
  {-# INLINE each #-}

_Point' :: Iso (Exp (Point v a)) (Exp (Point v b)) (Exp (v a)) (Exp (v b))
_Point' = iso unP_ P_

origin :: (Elt (v a), Additive v, P.Num a) => Exp (Point v a)
origin = P_ zero

class (L.Affine v) => Affine v where
  (.-.) ::
    forall a.
    (Num a, Box v a, Box (Diff v) a) =>
    Exp (v a) ->
    Exp (v a) ->
    Exp (Diff v a)
  infixl 6 .-.
  (.-.) = lift2 $ (L..-.) @v @(Exp a)

  (.+^) ::
    forall a.
    (Num a, Box v a, Box (Diff v) a) =>
    Exp (v a) ->
    Exp (Diff v a) ->
    Exp (v a)
  infixl 6 .+^
  (.+^) = lift2 $ (L..+^) @v @(Exp a)

  infixl 6 .-^
  (.-^) ::
    forall a.
    (Num a, Box v a, Box (Diff v) a) =>
    Exp (v a) ->
    Exp (Diff v a) ->
    Exp (v a)
  (.-^) = lift2 $ (L..-^) @v @(Exp a)
  {-# INLINE (.-^) #-}

instance Affine V0

instance Affine V1

instance Affine V2

instance Affine V3

instance Affine V4

instance (L.Affine l, L.Affine r) => Affine (Product l r)

instance Affine Plucker

instance Affine Quaternion

instance (Additive v) => Affine (Point v)

instance (A.Eq (v a)) => A.Eq (Point v a) where
  P_ x == P_ y = x A.== y
  {-# INLINE (==) #-}
  P_ x /= P_ y = x A./= y
  {-# INLINE (/=) #-}

instance (A.Ord (v a)) => A.Ord (Point v a) where
  P_ x `compare` P_ y = x `A.compare` y
  {-# INLINE compare #-}
  P_ x < P_ y = x A.< y
  {-# INLINE (<) #-}
  P_ x <= P_ y = x A.<= y
  {-# INLINE (<=) #-}
  P_ x > P_ y = x A.> y
  {-# INLINE (>) #-}
  P_ x >= P_ y = x A.>= y
  {-# INLINE (>=) #-}
  max (P_ x) (P_ y) = P_ $ A.max x y
  {-# INLINE max #-}
  min (P_ x) (P_ y) = P_ $ A.min x y
  {-# INLINE min #-}

instance (A.Bounded (v a)) => P.Bounded (Exp (Point v a)) where
  minBound = P_ A.minBound
  {-# INLINE minBound #-}
  maxBound = P_ A.maxBound
  {-# INLINE maxBound #-}

instance
  (A.Functor v, forall a. (Elt a) => Elt (v a)) =>
  A.Functor (Point v)
  where
  fmap f (P_ v) = P_ (A.fmap f v)
  {-# INLINE fmap #-}
  x <$ P_ v = P_ (x A.<$ v)
  {-# INLINE (<$) #-}

instance
  (A.Num (v a)) =>
  P.Num (Exp (Point v a))
  where
  (+) = P.fmap P_ . ((+) `on` unP_)
  {-# INLINE (+) #-}
  (-) = P.fmap P_ . ((-) `on` unP_)
  {-# INLINE (-) #-}
  (*) = P.fmap P_ . ((*) `on` unP_)
  {-# INLINE (*) #-}
  abs = P_ . A.abs . unP_
  {-# INLINE abs #-}
  signum = P_ . signum . unP_
  {-# INLINE signum #-}
  fromInteger = P_ . A.fromInteger
  {-# INLINE fromInteger #-}
  negate = P_ . A.negate . unP_
  {-# INLINE negate #-}

instance
  (A.Fractional (v a)) =>
  P.Fractional (Exp (Point v a))
  where
  (/) = P.fmap P_ . ((/) `on` unP_)
  {-# INLINE (/) #-}
  recip = P_ . recip . unP_
  {-# INLINE recip #-}
  fromRational = P_ . fromRational
  {-# INLINE fromRational #-}

instance
  (A.Floating (v a)) =>
  P.Floating (Exp (Point v a))
  where
  pi = P_ pi
  {-# INLINE pi #-}
  log = P_ . log . unP_
  {-# INLINE log #-}
  logBase = P.fmap P_ . (logBase `on` unP_)
  {-# INLINE logBase #-}
  exp = P_ . exp . unP_
  {-# INLINE exp #-}
  sqrt = P_ . sqrt . unP_
  {-# INLINE sqrt #-}
  (**) = P.fmap P_ . ((**) `on` unP_)
  {-# INLINE (**) #-}
  sin = P_ . sin . unP_
  {-# INLINE sin #-}
  tan = P_ . tan . unP_
  {-# INLINE tan #-}
  cos = P_ . cos . unP_
  {-# INLINE cos #-}
  asin = P_ . asin . unP_
  {-# INLINE asin #-}
  atan = P_ . atan . unP_
  {-# INLINE atan #-}
  acos = P_ . acos . unP_
  {-# INLINE acos #-}
  sinh = P_ . sinh . unP_
  {-# INLINE sinh #-}
  tanh = P_ . tanh . unP_
  {-# INLINE tanh #-}
  cosh = P_ . cosh . unP_
  {-# INLINE cosh #-}
  asinh = P_ . asinh . unP_
  {-# INLINE asinh #-}
  atanh = P_ . atanh . unP_
  {-# INLINE atanh #-}
  acosh = P_ . acosh . unP_
  {-# INLINE acosh #-}
  log1p = P_ . P.log1p . unP_
  {-# INLINE log1p #-}
  expm1 = P_ . P.expm1 . unP_
  {-# INLINE expm1 #-}
  log1pexp = P_ . P.log1pexp . unP_
  {-# INLINE log1pexp #-}
  log1mexp = P_ . P.log1mexp . unP_
  {-# INLINE log1mexp #-}

instance (A.Num (v a), (Epsilon (v a))) => Epsilon (Point v a) where
  nearZero = nearZero . unP_
  {-# INLINE nearZero #-}
