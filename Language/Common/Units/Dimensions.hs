{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Language.Common.Units.Dimensions
Description : Representation of abstract dimensions
Copyright   : (c) Galois, Inc. 2021
License     : N/A
Maintainer  : cphifer@galois.com
Stability   : experimental
Portability : N/A

Definitions relating to the representation of abstract dimensions.
-}

module Language.Common.Units.Dimensions where

import           Data.Text                    (Text, unpack)

import           Language.Common.Units.Factor (Factor (..))

-- | The type of abstract dimensions, stored simply as their name
data Dimension = Dimension
  { dimensionName    :: Text
  , dimensionFactors :: [Factor]
  }
  deriving(Eq, Ord)

instance Show Dimension where
  show Dimension { dimensionName = nm } = unpack nm

-- | Create a new @Dimension@
mkDimension :: Text -> Dimension
mkDimension nm = Dimension
  { dimensionName = nm
  , dimensionFactors = [F nm 1]
  }

-- | Special dimension: Dimensionless
dimensionless :: Dimension
dimensionless = Dimension
  { dimensionName = "Dimensionless"
  , dimensionFactors = []
  }
