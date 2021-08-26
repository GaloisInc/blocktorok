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
  { dimensionName = T.concat ["(", nm1, " :* ", nm2, ")"]
  , dimensionFactors = normalize $ fs1 @+ fs2
  }
  where
    (nm1, nm2) = (dimensionName d1, dimensionName d2)
    (fs1, fs2) = (dimensionFactors d1, dimensionFactors d2)

infixl 7 ||*
-- | Multiply two units to get another unit
(||*) :: Unit -> Unit -> Unit
u1 ||* u2 = Unit
  { unitName = T.concat ["(", nm1, " :* ", nm2, ")"]
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

infixl 7 |/
-- | Divide two dimensions to get another dimension
(|/) :: Dimension -> Dimension -> Dimension
d1 |/ d2 = Dimension
  { dimensionName = T.concat ["(", nm1, " :/ ", nm2, ")"]
  , dimensionFactors = normalize $ fs1 @- fs2
  }
  where
    (nm1, nm2) = (dimensionName d1, dimensionName d2)
    (fs1, fs2) = (dimensionFactors d1, dimensionFactors d2)

infixl 7 ||/
-- | Divide two units to get another unit
(||/) :: Unit -> Unit -> Unit
u1 ||/ u2 = Unit
  { unitName = T.concat ["(", nm1, " :/ ", nm2, ")"]
  , unitShowName = Nothing
  , unitBaseUnit = Nothing
  , unitDimension = dim1 |/ dim2
  , unitConversionRatio = undefined
  , unitFactors = normalize $ fs1 @- fs2
  , unitCanonicalConvRatio = convRatio1 / convRatio2
  }
  where
    (nm1, nm2) = (unitName u1, unitName u2)
    (dim1, dim2) = (unitDimension u1, unitDimension u2)
    (fs1, fs2) = (unitFactors u1, unitFactors u2)
    (convRatio1, convRatio2) = (unitCanonicalConvRatio u1, unitCanonicalConvRatio u2)

infixl 7 |^
-- | Raise a dimension to a power
(|^) :: Dimension -> Integer -> Dimension
d |^ power = Dimension
  { dimensionName = T.concat [dimensionName d, " :^ ", T.pack $ show power]
  , dimensionFactors = normalize $ dimensionFactors d @* power
  }

infixl 7 ||^
-- Raise a unit to a power
(||^) :: Unit -> Integer -> Unit
u ||^ power = Unit
  { unitName = T.concat [unitName u, " :^ ", T.pack $ show power]
  , unitShowName = Nothing
  , unitBaseUnit = Nothing
  , unitDimension = unitDimension u |^ power
  , unitConversionRatio = undefined
  , unitFactors = normalize $ unitFactors u @* power
  , unitCanonicalConvRatio = unitCanonicalConvRatio u ^^ power
  }

infixr 9 ||@
-- Multiply a unit by a prefix
(||@) :: Rational -> Unit -> Unit
prefix ||@ u =
  mkDerivedUnit (T.concat ["<prefix> ", " :@ ", unitName u])
                u
                prefix
                Nothing
