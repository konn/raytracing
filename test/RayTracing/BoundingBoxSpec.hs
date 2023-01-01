{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -O -dsuppress-all -dno-suppress-type-signatures -fplugin=Test.Tasty.Inspection.Plugin -Wno-missing-export-lists #-}

module RayTracing.BoundingBoxSpec where

import Data.Foldable (foldlM)
import RayTracing.BoundingBox (BoundingBox (..), hitsBox)
import RayTracing.Ray (Ray (..))
import Test.Tasty
import Test.Tasty.Inspection

hits :: Ray -> BoundingBox -> Maybe Double -> Maybe Double -> Bool
{-# INLINE hits #-}
hits = hitsBox

test_hitsBox :: TestTree
test_hitsBox =
  testGroup
    "hitsBox"
    [ $(inspectTest $ hasNoTypeClasses 'hits)
    , $(inspectTest $ mkObligation 'hits $ NoUseOf ['foldlM])
    , $(inspectTest $ mkObligation 'hits CoreOf)
    ]
