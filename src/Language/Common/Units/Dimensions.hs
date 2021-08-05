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

import Data.Text (Text)

import Language.Common.Units.Factor

-- | The type of abstract dimensions, stored simply as their name
newtype Dimension = Dimension { unDimension :: Text }

-- | A list of factors representing the dimension. Trivial as dimensions are
-- abstract entities; the list is always a singleton
dimFactorsOf :: Dimension -> [Factor Dimension]
dimFactorsOf d = [F d 1]
