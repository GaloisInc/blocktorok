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

import Data.Tuple (swap)

import Language.AST
import Physics.Model

-- | Return true if and only if the set of all couplings in the given LINK
--   program accounts for all pairs of distinct models.
hasAllCouplings :: Prog -> Bool
hasAllCouplings p = all represented modelPairs
  where
    couplings :: [(String, String)]
    couplings = (\(Coupling a b) -> (a, b)) <$> getCouplings p

    modelIDs :: [String]
    modelIDs = getID <$> getModels p

    modelPairs :: [(String, String)]
    modelPairs = [(a, b) | a <- modelIDs, b <- modelIDs, a /= b]

    represented :: (String, String) -> Bool
    represented mIDs = (mIDs `elem` couplings) || (swap mIDs `elem` couplings)
