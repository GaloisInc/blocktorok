{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Language.Common.Units.Combinators
Description : Functions to build new units/dimensions from old
Copyright   : (c) Galois, Inc. 2021
License     : N/A
Maintainer  : cphifer@galois.com
Stability   : experimental
Portability : N/A

Operators for building new unit values from old ones (e.g. multiplication)
-}

module Language.Common.Units.Combinators where

import qualified Data.Text as T

import Language.Common.Units.Dimensions
import Language.Common.Units.Factor
import Language.Common.Units.Units

infixl 7 |*
-- | Multiply two dimensions to get another dimension
(|*) :: Dimension -> Dimension -> Dimension
d1 |* d2 = Dimension
  { dimensionName = T.concat [nm1, " :* ", nm2]
  , dimensionFactors = normalize $ fs1 @+ fs2
  }
  where
    (nm1, nm2) = (dimensionName d1, dimensionName d2)
    (fs1, fs2) = (dimensionFactors d1, dimensionFactors d2)

infixl 7 ||*
-- | Multiply two units to get another unit
(||*) :: Unit -> Unit -> Unit
u1 ||* u2 = Unit
  { unitName = T.concat [nm1, " :* ", nm2]
  , unitShowName = Nothing
  , unitBaseUnit = Nothing -- Treat as canonical
  , unitDimension = dim1 |* dim2
  , unitConversionRatio = undefined -- Don't use this!
  , unitFactors = normalize $ fs1 @+ fs2
  , unitCanonicalConvRatio = convRatio1 * convRatio2
  }
  where
    (nm1, nm2) = (unitName u1, unitName u2)
    (dim1, dim2) = (unitDimension u1, unitDimension u2)
    (fs1, fs2) = (unitFactors u1, unitFactors u2)
    (convRatio1, convRatio2) = (unitCanonicalConvRatio u1, unitCanonicalConvRatio u2)
