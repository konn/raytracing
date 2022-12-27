{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnliftedDatatypes #-}

module RayTracing.BVH (
  BVH (),
  nearestHit,
  fromObjects,
  BVHScene' (..),
  Scene,
  Object (..),
  SomeObject,
  rayColour,
) where

import Control.Applicative ((<|>))
import Control.Foldl qualified as L
import Control.Lens (view)
import Control.Monad (forM_, guard)
import Control.Monad.ST.Strict
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Functor ((<&>))
import Data.Image.Types (Pixel)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Vector qualified as V
import Data.Vector.Algorithms.Intro qualified as Intro
import Data.Vector.Algorithms.Optimal qualified as OptSort
import Data.Vector.Generic.Mutable qualified as MG
import Data.Vector.Hybrid qualified as HV
import Data.Vector.Hybrid.Mutable qualified as HMV
import Data.Vector.Unboxed qualified as U
import Data.Word (Word8)
import GHC.Generics
import Linear (_x, _y, _z)
import RayTracing.BoundingBox
import RayTracing.Object (Object (..), SomeObject)
import RayTracing.Object.Material
import RayTracing.Object.Shape
import RayTracing.Ray (Ray)
import System.Random (RandomGen)
import System.Random.Stateful (RandomGenM, STGenM, randomRM, runSTGen)

data BVH a
  = Leaf !BoundingBox !a
  | Branch !BoundingBox !(BVH a) !(BVH a)
  | Empty
  deriving (Show, Eq, Ord, Generic, Generic1, Functor, Foldable, Traversable)

fromObjects :: (Foldable t, RandomGen g, Hittable a) => t a -> g -> (BVH a, g)
fromObjects hits
  | null hits = (Empty,)
  | otherwise = flip runSTGen $ \g -> do
      fmap fst . buildBVH' g
        =<< HV.unsafeThaw
        =<< L.foldM
          ( L.premapM
              (pure . ((,) <$> id <*> boundingBox'))
              L.vectorM
          )
          hits

buildBVH' :: (RandomGen g) => STGenM g s -> HV.MVector V.MVector U.MVector s (a, BoundingBox) -> ST s (BVH a, BoundingBox)
buildBVH' g = go
  where
    go objs = do
      i <- randomRM (0 :: Word8, 2) g
      let cmp = case i of
            0 -> comparing $ view _x . lowerBound . snd
            1 -> comparing $ view _y . lowerBound . snd
            _ -> comparing $ view _z . lowerBound . snd
      case HMV.length objs of
        0 -> pure (Empty, error "Empty")
        1 ->
          HMV.unsafeRead objs 0 <&> \(n, bb) ->
            (Leaf bb n, bb)
        2 -> do
          OptSort.sort2ByOffset cmp objs 0
          (l, lb) <- HMV.unsafeRead objs 0
          (r, rb) <- HMV.unsafeRead objs 1
          let !lrb = lb <> rb
          pure (Branch lrb (Leaf lb l) (Leaf rb r), lrb)
        3 -> do
          OptSort.sort3ByOffset cmp objs 0
          (l, lb) <- HMV.unsafeRead objs 0
          (m, mb) <- HMV.unsafeRead objs 1
          (r, rb) <- HMV.unsafeRead objs 2
          let !lmb = lb <> mb
              !lmrb = lmb <> rb
          pure (Branch lmrb (Branch lmb (Leaf lb l) (Leaf mb m)) (Leaf rb r), lmrb)
        4 -> do
          OptSort.sort4ByOffset cmp objs 0
          (o0, bb0) <- HMV.unsafeRead objs 0
          (o1, bb1) <- HMV.unsafeRead objs 1
          (o2, bb2) <- HMV.unsafeRead objs 2
          (o3, bb3) <- HMV.unsafeRead objs 3
          let !b01 = bb0 <> bb1
              !b23 = bb2 <> bb3
              !b0123 = b01 <> b23
          pure
            ( Branch
                b0123
                (Branch b01 (Leaf bb0 o0) (Leaf bb1 o1))
                (Branch b23 (Leaf bb2 o2) (Leaf bb3 o3))
            , b0123
            )
        size -> do
          Intro.sortBy cmp objs
          let loSz = size `quot` 2
              lo = HMV.unsafeSlice 0 loSz objs
              hi = MG.unsafeSlice loSz (size - loSz) objs
          (l, lb) <- go lo
          (r, rb) <- go hi
          let !lrb = lb <> rb
          pure (Branch lrb l r, lrb)

boundingBox' :: Hittable obj => obj -> BoundingBox
boundingBox' = fromMaybe (error "BoundingBox not found!") . boundingBox

nearestHit ::
  Hittable a =>
  Maybe Double ->
  Maybe Double ->
  Ray ->
  BVH a ->
  Maybe (HitRecord, a)
nearestHit mtmin mtmax ray = go
  where
    go Empty = Nothing
    go tree = do
      forM_ (bvhBBox tree) $ \bbox ->
        guard $ hitsBox ray bbox mtmin mtmax
      case tree of
        Leaf _ a -> (,a) <$> hitWithin a mtmin mtmax ray
        Branch _ l r -> go l <|> go r

bvhBBox :: BVH a -> Maybe BoundingBox
bvhBBox (Leaf bbox _) = Just bbox
bvhBBox (Branch bbox _ _) = Just bbox
bvhBBox Empty = Nothing

type Scene = BVHScene' SomeHittable SomeMaterial

data BVHScene' sh mat = Scene
  { objects :: !(BVH (Object sh mat))
  , background :: !(Ray -> Pixel Double)
  }
  deriving (Generic, Generic1, Functor, Foldable, Traversable)

rayColour ::
  ( Hittable sh
  , Material mat
  , RandomGenM g r m
  ) =>
  -- | Threshould to reagard as zero
  Double ->
  BVHScene' sh mat ->
  g ->
  Int ->
  Ray ->
  m (Pixel Double)
rayColour eps Scene {..} g = go
  where
    go !depth r
      | depth <= 0 = pure 0.0
      | Just (hit, obj) <- nearestHit (Just eps) Nothing r objects = do
          runMaybeT (scatter obj hit r g) >>= \case
            Nothing -> pure 0.0
            Just (attenuation, scattered) ->
              (attenuation .*) <$> go (depth - 1) scattered
      | otherwise = pure $ background r
