{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Language.Common.Units.Units
Description : Representation of units
Copyright   : (c) Galois, Inc. 2021
License     : N/A
Maintainer  : cphifer@galois.com
Stability   : experimental
Portability : N/A

Definitions relating to the representation of units.
-}

module Language.Common.Units.Units where

import           Data.Text                        (Text, unpack)

import           Language.Common.Units.Dimensions (Dimension, dimensionless)
import           Language.Common.Units.Factor     (Factor (..))

import           Prettyprinter                    (Pretty (pretty))

-- | The type of units
data Unit = Unit
  { -- | The name of this unit.
    unitName               :: Text
    -- | How this unit should be displayed. If @Nothing@, uses @unitName@.
  , unitShowName           :: Maybe Text
    -- | The base unit. @Nothing@ indicates that this unit is canonical.
  , unitBaseUnit           :: Maybe Unit
    -- | The dimension of this unit. Must be defined if @unitBaseUnit@ is @Nothing@.
  , unitDimension          :: Dimension
    -- | Conversion ratio from the base unit to this unit. If this is a
    -- canonical unit, this must be 1.
  , unitConversionRatio    :: Rational
    -- | The canonical factors comprising this unit.
  , unitFactors            :: [Factor]
    -- | Conversion ration from the underlying canonical unit to this one.
    -- Computed by multiplying all the ratios.
  , unitCanonicalConvRatio :: Rational
  }
  deriving(Eq, Ord)

instance Show Unit where
  show Unit { unitName = nm, unitShowName = showNm } =
    case showNm of
      Nothing      -> unpack nm
      Just showNm' -> unpack showNm'

instance Pretty Unit where
  pretty Unit { unitName = nm, unitShowName = showNm } =
    case showNm of
      Nothing      -> pretty nm
      Just showNm' -> pretty showNm'

-- | Create a new canonical unit
mkCanonicalUnit :: Text -> Dimension -> Maybe Text -> Unit
mkCanonicalUnit nm dim showNm = Unit
  { unitName = nm
  , unitShowName = showNm
  , unitBaseUnit = Nothing
  , unitDimension = dim
  , unitConversionRatio = 1
  , unitFactors = [F nm 1]
  , unitCanonicalConvRatio = 1
  }

-- | Create a new derived unit
mkDerivedUnit :: Text -> Unit -> Rational -> Maybe Text -> Unit
mkDerivedUnit nm baseUnit ratio showNm = Unit
  { unitName = nm
  , unitShowName = showNm
  , unitBaseUnit = Just baseUnit
  , unitDimension = unitDimension baseUnit
  , unitConversionRatio = ratio
  , unitFactors = unitFactors baseUnit
  , unitCanonicalConvRatio = ratio * unitCanonicalConvRatio baseUnit
  }

-- | Compute the canonical unit of the given unit
canonicalUnit :: Unit -> Unit
canonicalUnit u@Unit { unitBaseUnit = mbaseUnit } = maybe u canonicalUnit mbaseUnit

-- | Special unit: Number
number :: Unit
number = Unit
  { unitName = "Number"
  , unitShowName = Nothing
  , unitBaseUnit = Nothing
  , unitDimension = dimensionless
  , unitConversionRatio = 1
  , unitFactors = []
  , unitCanonicalConvRatio = 1
  }
