{-|
Module      : Language.Check
Description : Some static checks on LINK programs
Copyright   : (c) Galois, Inc. 2020
License     : N/A
Maintainer  : cphifer@galois.com
Stability   : experimental
Portability : N/A

This module holds some simple static checking that can be done on LINK
programs. They do not constitute full type-checking of any sort; that
work is currently TBD and will likely consist of some cool stuff with
ologs and applied category theory more generally.
-}

module Language.Check
  ( hasAllCouplings
  ) where

import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Language.AST

-- | Return true if and only if the set of all couplings in the given LINK
--   program accounts for all pairs of distinct models.
hasAllCouplings :: Prog -> Bool
hasAllCouplings p = couplings == modelPairs
  where
    modelNames :: Set Identifier
    modelNames = Map.keysSet $ getModels p

    modelPairs :: Set (Set Identifier)
    modelPairs = Set.filter (\s -> Set.size s == 2) $ Set.powerSet modelNames

    couplings :: Set (Set Identifier)
    couplings = Set.fromList $ Set.fromList . (\(Coupling a b) -> [a, b]) <$> getCouplings p
