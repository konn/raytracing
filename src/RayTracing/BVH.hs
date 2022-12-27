{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UnliftedDatatypes #-}

module RayTracing.BVH (
  BVH (),
  fromObjects,
) where

import Control.Applicative ((<|>))
import Control.Foldl qualified as L
import Control.Lens (view)
import Control.Monad (forM_, guard)
import Control.Monad.ST.Strict
import Data.Bifunctor qualified as Bi
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
import RayTracing.Object.Shape
import System.Random (RandomGen)
import System.Random.Stateful (STGenM, randomRM, runSTGen)

data BVH a
  = Leaf !a
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
        1 -> Bi.first Leaf <$> HMV.unsafeRead objs 0
        2 -> do
          OptSort.sort2ByOffset cmp objs 0
          (l, lb) <- HMV.unsafeRead objs 0
          (r, rb) <- HMV.unsafeRead objs 1
          let !lrb = lb <> rb
          pure (Branch lrb (Leaf l) (Leaf r), lrb)
        3 -> do
          OptSort.sort3ByOffset cmp objs 0
          (l, lb) <- HMV.unsafeRead objs 0
          (m, mb) <- HMV.unsafeRead objs 1
          (r, rb) <- HMV.unsafeRead objs 2
          let !lmb = lb <> mb
              !lmrb = lmb <> rb
          pure (Branch lmrb (Branch lmb (Leaf l) (Leaf m)) (Leaf r), lmrb)
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
                (Branch b01 (Leaf o0) (Leaf o1))
                (Branch b23 (Leaf o2) (Leaf o3))
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

instance Hittable a => Hittable (BVH a) where
  hitWithin Empty _ _ _ = Nothing
  hitWithin tree mtmin mtmax ray = do
    forM_ (bvhBBox tree) $ \bbox ->
      guard $ hitsBox ray bbox mtmin mtmax
    case tree of
      Leaf a -> hitWithin a mtmin mtmax ray
      Branch _ l r ->
        hitWithin l mtmin mtmax ray
          <|> hitWithin r mtmin mtmax ray
  boundingBox = bvhBBox

bvhBBox :: Hittable a => BVH a -> Maybe BoundingBox
bvhBBox (Leaf a) = boundingBox a
bvhBBox (Branch bbox _ _) = Just bbox
bvhBBox Empty = Nothing
