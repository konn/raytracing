{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnliftedDatatypes #-}

module RayTracing.BVH (
  BVH (),
  depth,
  size,
  nearestHit,
  fromObjects,
  BVHScene' (..),
  Scene,
  Object (..),
  SomeObject,
  rayColour,
) where

import Control.Arrow (Arrow ((&&&)))
import Control.Foldl qualified as L
import Control.Lens (view)
import Control.Monad (forM_, guard)
import Control.Monad.ST.Strict
import Control.Monad.Trans.Maybe (MaybeT (..))
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
  | Branch !Int !BoundingBox !(BVH a) !(BVH a)
  | Empty
  deriving (Show, Eq, Ord, Generic, Generic1, Functor, Foldable)

depth :: BVH a -> Int
{-# INLINE depth #-}
depth = go
  where
    {-# INLINE go #-}
    go Empty = 0
    go Leaf {} = 1
    go (Branch _ _ l r) =
      max (go l) (go r) + 1

fromObjects :: (Foldable t, RandomGen g, Hittable a) => t a -> g -> (BVH a, g)
{-# INLINE fromObjects #-}
fromObjects hits
  | null hits = (Empty,)
  | otherwise = flip runSTGen $ \g -> do
      buildBVH' g
        =<< HV.unsafeThaw
        =<< L.foldM
          ( L.premapM
              (pure . ((,) <$> boundingBox' <*> id))
              L.vectorM
          )
          hits

buildBVH' ::
  (RandomGen g) =>
  STGenM g s ->
  HV.MVector U.MVector V.MVector s (BoundingBox, a) ->
  ST s (BVH a)
{-# INLINE buildBVH' #-}
buildBVH' g = fmap snd . go
  where
    {-# INLINE go #-}
    go objs = do
      i <- randomRM (0 :: Word8, 2) g
      let cmp = case i of
            0 -> comparing $ view _x . lowerBound . fst
            1 -> comparing $ view _y . lowerBound . fst
            _ -> comparing $ view _z . lowerBound . fst
      case HMV.length objs of
        0 -> pure (error "Empty", Empty)
        1 ->
          (fst &&& uncurry Leaf) <$> HMV.unsafeRead objs 0
        2 -> do
          OptSort.sort2ByOffset cmp objs 0
          (lb, l) <- HMV.unsafeRead objs 0
          (rb, r) <- HMV.unsafeRead objs 1
          let !lrb = lb <> rb
          pure (lrb, Branch 2 lrb (Leaf lb l) (Leaf rb r))
        3 -> do
          OptSort.sort3ByOffset cmp objs 0
          (lb, l) <- HMV.unsafeRead objs 0
          (mb, m) <- HMV.unsafeRead objs 1
          (rb, r) <- HMV.unsafeRead objs 2
          let !lmb = lb <> mb
              !lmrb = lmb <> rb
          pure (lmrb, Branch 3 lmrb (Branch 2 lmb (Leaf lb l) (Leaf mb m)) (Leaf rb r))
        4 -> do
          OptSort.sort4ByOffset cmp objs 0
          (bb0, o0) <- HMV.unsafeRead objs 0
          (bb1, o1) <- HMV.unsafeRead objs 1
          (bb2, o2) <- HMV.unsafeRead objs 2
          (bb3, o3) <- HMV.unsafeRead objs 3
          let !b01 = bb0 <> bb1
              !b23 = bb2 <> bb3
              !b0123 = b01 <> b23
          pure
            ( b0123
            , Branch
                4
                b0123
                (Branch 2 b01 (Leaf bb0 o0) (Leaf bb1 o1))
                (Branch 2 b23 (Leaf bb2 o2) (Leaf bb3 o3))
            )
        len -> do
          Intro.sortBy cmp objs
          let loSz = len `quot` 2
              lo = HMV.unsafeSlice 0 loSz objs
              hi = MG.unsafeSlice loSz (len - loSz) objs
          (lb, l) <- go lo
          (rb, r) <- go hi
          let !lrb = lb <> rb
          pure (lrb, Branch (size l + size r) lrb l r)

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
nearestHit mtmin mtmax0 ray = go mtmax0
  where
    {-# INLINE go #-}
    go _ Empty = Nothing
    go mtmax tree = do
      forM_ (bvhBBox tree) $ \bbox ->
        guard $ hitsBox ray bbox mtmin mtmax
      case tree of
        Leaf _ a -> (,a) <$> hitWithin a mtmin mtmax ray
        Branch _ _ l r ->
          case go mtmax l of
            Just hit@(Hit {hitTime}, _) ->
              Just $! fromMaybe hit (go (Just hitTime) r)
            Nothing -> go mtmax r

bvhBBox :: BVH a -> Maybe BoundingBox
{-# INLINE bvhBBox #-}
bvhBBox (Leaf bbox _) = Just bbox
bvhBBox (Branch _ bbox _ _) = Just bbox
bvhBBox Empty = Nothing

size :: BVH a -> Int
{-# INLINE size #-}
size (Leaf _ _) = 1
size (Branch n _ _ _) = n
size Empty = 0

type Scene = BVHScene' SomeHittable SomeMaterial

data BVHScene' sh mat = Scene
  { objects :: !(BVH (Object sh mat))
  , background :: !(Ray -> Pixel Double)
  }
  deriving (Generic, Generic1, Functor, Foldable)

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
{-# INLINE rayColour #-}
rayColour eps Scene {..} g = go
  where
    {-# INLINE go #-}
    go !lvl r
      | lvl <= 0 = pure 0.0
      | Just (hit, obj) <- nearestHit (Just eps) Nothing r objects = do
          runMaybeT (scatter obj hit r g) >>= \case
            Nothing -> pure 0.0
            Just (attenuation, scattered) ->
              (attenuation .*) <$> go (lvl - 1) scattered
      | otherwise = pure $ background r
