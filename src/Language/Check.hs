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
  , allVarsDeclared
  ) where

import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Link.AST (Coupling(..), Prog(..))
import Data.Link.Identifier (Identifier)
-- import Math
-- import Physics.Model

-- | Return true if and only if the set of all couplings in the given LINK
--   program accounts for all pairs of distinct models.
hasAllCouplings :: Prog -> Bool
hasAllCouplings p = couplings == modelPairs
  where
    modelPairs :: Set (Set Identifier)
    modelPairs = Set.filter (\s -> Set.size s == 2) $ Set.powerSet $ Map.keysSet $ getModels p

    couplings :: Set (Set Identifier)
    couplings = Set.fromList $ Set.fromList . (\(Coupling _ a b _ _ _ _ _) -> [a, b]) <$> getCouplings p

-- | Return true if and only if, for each model, the set of variables appearing
--   in the model's equations have been declared.
allVarsDeclared :: Prog -> Bool
allVarsDeclared _ = True -- FIXME
-- allVarsDeclared2 p = all modelVarsDeclared $ getModels p
--   where
--     modelVarsDeclared :: Model -> Bool
--     modelVarsDeclared m = eqVars `Set.isSubsetOf` Set.union declaredConsts (Physics.Model.getVars m)
--       where
--         declaredConsts :: Set Identifier
--         declaredConsts = Set.fromList $ Map.keys $ getConsts m

--         eqVars :: Set Identifier
--         eqVars = foldr Set.union Set.empty $ vars <$> (getLHS <$> Physics.Model.getEqs m) ++ (getRHS <$> Physics.Model.getEqs m)
