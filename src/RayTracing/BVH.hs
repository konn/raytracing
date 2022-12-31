{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module RayTracing.BVH (
  BVH (),
  depth,
  size,
  nearestHit,
  fromObjects,
  fromObjectsWithBucket,
  BVHScene' (..),
  Scene,
  Object (..),
  SomeObject,
  rayColour,
) where

import Control.Foldl qualified as L
import Control.Lens (view)
import Control.Monad (forM_, guard)
import Control.Monad.ST.Strict
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.State.Strict (State)
import Data.Image.Types (Pixel, RGB)
import Data.Kind (Constraint, Type)
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
import GHC.Exts (UnliftedType)
import GHC.Generics
import Linear (_x, _y, _z)
import RayTracing.BoundingBox
import RayTracing.Object (Object (..), SomeObject)
import RayTracing.Object.Material
import RayTracing.Object.Shape
import RayTracing.Ray (Ray)
import System.Random (RandomGen)
import System.Random.Stateful (STGenM, randomRM, runSTGen)

type BVH :: Type -> Type
newtype BVH a = BVH {unBVH :: (() :: Constraint) => BVH# a}

type BVH# :: Type -> UnliftedType
data BVH# a where
  Leaf# :: !BoundingBox -> !(V.Vector a) -> BVH# a
  Branch# :: !Int -> !BoundingBox -> !(BVH# a) -> !(BVH# a) -> BVH# a
  Empty# :: BVH# a

{-# INLINE leaf1 #-}
leaf1 :: BoundingBox -> p -> BVH# p
leaf1 bb v = Leaf# bb (V.singleton v)

depth :: BVH a -> Int
{-# INLINE depth #-}
depth bvh = go (unBVH bvh)
  where
    {-# INLINE go #-}
    go :: BVH# b -> Int
    go Empty# = 0
    go Leaf# {} = 1
    go (Branch# _ _ l r) =
      max (go l) (go r) + 1

fromObjects :: (Foldable t, RandomGen g, Hittable a) => t a -> g -> (BVH a, g)
{-# INLINE fromObjects #-}
fromObjects = fromObjectsWithBucket 4

fromObjectsWithBucket :: (Foldable t, RandomGen g, Hittable a) => Int -> t a -> g -> (BVH a, g)
{-# INLINE fromObjectsWithBucket #-}
fromObjectsWithBucket siz hits
  | null hits = (BVH Empty#,)
  | otherwise = flip runSTGen $ \g -> do
      buildBVH' (max 1 siz) g
        =<< HV.unsafeThaw
        =<< L.foldM
          ( L.premapM
              (pure . ((,) <$> boundingBox' <*> id))
              L.vectorM
          )
          hits

data P a = P
  { _bbox :: {-# UNPACK #-} !BoundingBox
  , bvh# :: !(BVH# a)
  }

buildBVH' ::
  (RandomGen g) =>
  Int ->
  STGenM g s ->
  HV.MVector U.MVector V.MVector s (BoundingBox, a) ->
  ST s (BVH a)
{-# INLINE buildBVH' #-}
buildBVH' bucketSize g = fmap (\p -> BVH (bvh# p)) . go
  where
    {-# INLINE go #-}
    go objs = do
      i <- randomRM (0 :: Word8, 2) g
      let cmp = case i of
            0 -> comparing $ view _x . lowerBound . fst
            1 -> comparing $ view _y . lowerBound . fst
            _ -> comparing $ view _z . lowerBound . fst
          len = HMV.length objs
      if
          | len <= 1 -> pure ()
          | len == 2 -> OptSort.sort2ByOffset cmp objs 0
          | len == 3 -> OptSort.sort3ByOffset cmp objs 0
          | len == 4 -> OptSort.sort4ByOffset cmp objs 0
          | otherwise -> Intro.sortBy cmp objs
      case len of
        _ | len <= bucketSize -> do
          bbObjs <- HV.unsafeFreeze objs
          let !bbs = U.foldl1' (<>) (HV.projectFst bbObjs)
              !objs' = HV.projectSnd bbObjs
          pure $ P bbs (Leaf# bbs objs')
        2 -> do
          (lb, l) <- HMV.unsafeRead objs 0
          (rb, r) <- HMV.unsafeRead objs 1
          let !lrb = lb <> rb
          pure $ P lrb (Branch# 2 lrb (leaf1 lb l) (leaf1 rb r))
        3 -> do
          (lb, l) <- HMV.unsafeRead objs 0
          (mb, m) <- HMV.unsafeRead objs 1
          (rb, r) <- HMV.unsafeRead objs 2
          objs' <- V.unsafeFreeze $ HMV.projectSnd objs
          let !lmb = lb <> mb
              !lmrb = lmb <> rb
              !ls
                | bucketSize == 1 = Branch# 2 lmb (leaf1 lb l) (leaf1 mb m)
                | otherwise = Leaf# lmb $ V.unsafeTake 2 objs'
              !rs = leaf1 rb r
          pure $ P lmrb (Branch# 3 lmrb ls rs)
        4 -> do
          (bb0, o0) <- HMV.unsafeRead objs 0
          (bb1, o1) <- HMV.unsafeRead objs 1
          (bb2, o2) <- HMV.unsafeRead objs 2
          (bb3, o3) <- HMV.unsafeRead objs 3
          objs' <- V.unsafeFreeze $ HMV.projectSnd objs
          let !b01 = bb0 <> bb1
              !b23 = bb2 <> bb3
              !b0123 = b01 <> b23
              !(# ls, rs #) =
                if bucketSize == 1
                  then
                    (#
                      Branch# 2 b01 (leaf1 bb0 o0) (leaf1 bb1 o1)
                      , Branch# 2 b23 (leaf1 bb2 o2) (leaf1 bb3 o3)
                    #)
                  else
                    (#
                      Leaf# b01 (V.unsafeTake 2 objs')
                      , Leaf# b23 (V.unsafeDrop 2 objs')
                    #)
          pure $ P b0123 (Branch# 4 b0123 ls rs)
        _ -> do
          let loSz = len `quot` 2
              lo = HMV.unsafeSlice 0 loSz objs
              hi = MG.unsafeSlice loSz (len - loSz) objs
          P lb l <- go lo
          P rb r <- go hi
          let !lrb = lb <> rb
          pure $ P lrb (Branch# (size# l + size# r) lrb l r)

boundingBox' :: Hittable obj => obj -> BoundingBox
boundingBox' = fromMaybe (error "BoundingBox not found!") . boundingBox

nearestHit ::
  Hittable a =>
  Maybe Double ->
  Maybe Double ->
  Ray ->
  BVH a ->
  Maybe (HitRecord, a)
{-# INLINE nearestHit #-}
nearestHit mtmin mtmax0 ray bvh = go mtmax0 (unBVH bvh)
  where
    {-# INLINE go #-}
    go _ Empty# = Nothing
    go mtmax tree = do
      forM_ (bvhBBox tree) $ \bbox ->
        guard $ hitsBox ray bbox mtmin mtmax
      case tree of
        Leaf# _ as -> withNearestHitWithin mtmin mtmax ray as
        Branch# _ _ l r ->
          case go mtmax l of
            Just hit@(Hit {hitTime}, _) ->
              Just $! fromMaybe hit (go (Just hitTime) r)
            Nothing -> go mtmax r

bvhBBox :: BVH# a -> Maybe BoundingBox
{-# INLINE bvhBBox #-}
bvhBBox (Leaf# bbox _) = Just bbox
bvhBBox (Branch# _ bbox _ _) = Just bbox
bvhBBox Empty# = Nothing

size :: BVH a -> Int
{-# INLINE size #-}
size bvh = size# (unBVH bvh)

size# :: BVH# a -> Int
size# (Leaf# _ v) = V.length v
size# (Branch# n _ _ _) = n
size# Empty# = 0

type Scene = BVHScene' SomeHittable SomeMaterial

data BVHScene' sh mat = Scene
  { objects :: !(BVH (Object sh mat))
  , background :: !(Ray -> Pixel RGB Double)
  }
  deriving (Generic)

rayColour ::
  ( Hittable sh
  , Material mat
  , RandomGen g
  ) =>
  -- | Threshould to reagard as zero
  Double ->
  BVHScene' sh mat ->
  Int ->
  Ray ->
  State g (Pixel RGB Double)
{-# INLINE rayColour #-}
rayColour eps Scene {..} = go
  where
    {-# INLINE go #-}
    go !lvl r
      | lvl <= 0 = pure 0.0
      | Just (hit, obj) <- nearestHit (Just eps) Nothing r objects = do
          runMaybeT (scatter obj hit r) >>= \case
            Nothing -> pure 0.0
            Just (attenuation, scattered) ->
              (attenuation .*) <$> go (lvl - 1) scattered
      | otherwise = pure $ background r
