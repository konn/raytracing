{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Numeric.Utils (pattern Infinity, pattern NegativeInfinity, pattern NaN) where

import Control.Arrow ((&&&))
import GHC.Float (isDoubleInfinite, isDoubleNaN)

pattern Infinity :: Double
pattern Infinity <- (isDoubleInfinite &&& (> 0) -> (1, True))
  where
    Infinity = 1 / 0

pattern NegativeInfinity :: Double
pattern NegativeInfinity <- (isDoubleInfinite &&& (< 0) -> (1, True))
  where
    NegativeInfinity = -1 / 0

pattern NaN :: Double
pattern NaN <- (isDoubleNaN -> 1)
  where
    NaN = 0 / 0
