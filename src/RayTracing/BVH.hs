{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas -fsimpl-tick-factor=1000 #-}

{-# HLINT ignore "Avoid lambda" #-}

module RayTracing.BVH (
  BVH (),
  depth,
  size,
  nearestHit,
  toFlatBVH,
  fromObjects,
  fromObjectsWithBinBucket,
) where

import Control.Applicative (Alternative (..), (<|>))
import Control.Foldl qualified as L
import Control.Lens (both, sumOf, view, (%~))
import Control.Monad (forM_, guard)
import Control.Monad.ST.Strict (ST, runST)
import Data.Function ((&))
import Data.Kind (Constraint, Type)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Semigroup (Arg (..), Max (..), Min (..))
import Data.Semigroup.Foldable (fold1)
import Data.Strict.Tuple (Pair (..))
import Data.Vector qualified as V
import Data.Vector.Algorithms.Intro qualified as Intro
import Data.Vector.Algorithms.Optimal qualified as OptSort
import Data.Vector.Generic.Mutable qualified as MG
import Data.Vector.Hybrid qualified as HV
import Data.Vector.Hybrid.Mutable qualified as HMV
import Data.Vector.Unboxed qualified as U
import Effectful
import Effectful.NonDet (NonDet)
import Effectful.State.Static.Local (State)
import GHC.Exts (UnliftedType)
import Linear (V3 (..), _x, _y, _z)
import Linear.Affine ((.-.))
import RayTracing.BoundingBox
import RayTracing.Object.Shape
import RayTracing.Ray (Ray)
import System.Random (RandomGen)

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

fromObjects :: (Foldable t, Hittable a) => t a -> BVH a
{-# INLINE fromObjects #-}
fromObjects = fromObjectsWithBinBucket 2 4

fromObjectsWithBinBucket :: (Foldable t, Hittable a) => Int -> Int -> t a -> BVH a
{-# INLINE fromObjectsWithBinBucket #-}
fromObjectsWithBinBucket bins siz hits
  | null hits = BVH Empty#
  | otherwise = runST $ do
      buildBVH' (max 2 bins) (max 1 siz)
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
  Int ->
  Int ->
  HV.MVector U.MVector V.MVector s (BoundingBox, a) ->
  ST s (BVH a)
{-# INLINE buildBVH' #-}
buildBVH' bins0 bucketSize = fmap (\p -> BVH (bvh# p)) . go
  where
    {-# INLINE go #-}
    go objs = do
      !bbs <- U.foldl1' (<>) <$> U.unsafeFreeze (HMV.projectFst objs)
      let Max (Arg _ !ax) =
            fold1 $
              fmap Max . Arg
                <$> (upperBound bbs .-. lowerBound bbs)
                <*> V3 _x _y _z
          !cmp = comparing $ view ax . lowerBound . fst
          !len = HMV.length objs
      if
          | len <= 1 -> pure ()
          | len == 2 -> OptSort.sort2ByOffset cmp objs 0
          | len == 3 -> OptSort.sort3ByOffset cmp objs 0
          | len == 4 -> OptSort.sort4ByOffset cmp objs 0
          | otherwise -> Intro.sortBy cmp objs
      case len of
        _ | len <= bucketSize -> do
          bbObjs <- HV.unsafeFreeze objs
          let !objs' = HV.projectSnd bbObjs
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
          let !bins = min bins0 len
              !(blk, rest) = len `quotRem` bins
          !bbs' <- U.unsafeFreeze $ HMV.projectFst objs
          let Min (Arg _ !loSz) =
                U.foldl1' (<>) $
                  U.unfoldrExactN
                    (bins - 1)
                    ( \(ith :!: off) ->
                        let !step
                              | ith < rest = blk + 1
                              | otherwise = blk
                            !k = off + step
                            !cost =
                              U.splitAt k bbs'
                                & both %~ surfaceArea . U.foldl1' (<>)
                                & sumOf both
                         in (Min (Arg cost k), (ith + 1) :!: k)
                    )
                    (0 :!: 0)
              !lo = HMV.unsafeSlice 0 loSz objs
              !hi = MG.unsafeSlice loSz (len - loSz) objs
          P lb l <- go lo
          P rb r <- go hi
          let !lrb = lb <> rb
          pure $ P lrb (Branch# (size# l + size# r) lrb l r)

boundingBox' :: Hittable obj => obj -> BoundingBox
boundingBox' = fromMaybe (error "BoundingBox not found!") . boundingBox

nearestHit ::
  (Hittable a, RandomGen g) =>
  Double ->
  Double ->
  Ray ->
  BVH a ->
  Eff '[NonDet, State g] (HitRecord, a)
{-# INLINE nearestHit #-}
nearestHit mtmin mtmax0 ray bvh = go mtmax0 (unBVH bvh)
  where
    {-# INLINE go #-}
    go _ Empty# = empty
    go mtmax tree = do
      forM_ (bvhBBox tree) $ \bbox ->
        guard $ hitsBox ray bbox mtmin mtmax
      case tree of
        Leaf# _ as -> withNearestHitWithin mtmin mtmax ray as
        Branch# _ _ l r ->
          ( do
              hit@(Hit {hitTime}, _) <- go mtmax l
              go hitTime r <|> pure hit
          )
            <|> go mtmax r

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

-- | Converts a list of objects into trivial flat (brute-force) BVH.
toFlatBVH :: (Foldable t, Hittable a) => t a -> BVH a
{-# INLINE toFlatBVH #-}
toFlatBVH objs
  | null objs = BVH Empty#
  | otherwise = fromMaybe (BVH Empty#) $ do
      bbox <- foldMap boundingBox objs
      pure $ BVH $ Leaf# bbox $ L.fold L.vector objs
