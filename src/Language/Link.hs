{-|
Module      : Language.Link
Description : Linking for the LINK language
Copyright   : Galois, Inc. 2021
License     : N/A
Maintainer  : cphifer@galois.com
Stability   : Experimental
Portability : N/A

This module defines functions for linking multiple LINK source files into a
single source for compilation. It provides some base-level static analyses
around model compatibility.

At its core, though, this linker is simply performing a union-like operation
on the source files - For two models in the sources with identical names, their
components will be collected, checked for consistency, and used to construct
a single model with all of the equations, variables, etc.
-}

module Language.Link
  ( link
  ) where

import Control.Monad (when)
import Control.Monad.Except (Except, throwError)

import Data.Link.AST (Config(..), Duration, Prog(..))
import Data.Link.Identifier (Identifier)
import Data.Physics.Model (Model)
import Data.Units.UnitExp (UnitExp)
import Language.Error (LinkError(..))

import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, catMaybes)
import qualified Data.Set as Set

import Language.Haskell.TH.Syntax (Name)

-- | Given a list of LINK programs, synthesize a single LINK program built by
-- taking a 'union' over everything in the list. This union is intelligent in
-- that it checks for certain kinds of compatibility problems, such as
-- model variables declaed with different units. The behavior of this function
-- will likely change as LINK evolves.
link :: [Prog] -> Except LinkError Prog
link []     = throwError NoPrograms
link [p]    = return p
link (p:ps) =
  do -- Destructure the first program to support the various checks
     let Prog { getConfig = Config { getGlobalStep = gs
                                   , getDuration = dur
                                   , getConsts = consts
                                   , getRunFn = rfn
                                   , getBackendConfig = bcConfig
                                   }
              , getModels = models
              , getCouplings = couplings } = p

     -- Check that all global steps are the same, throwing an error otherwise
     checkMatch gs gSteps MismatchedGSs
     checkMatch dur durs MismatchedDur

     -- Unify config constants
     allConsts <- linkCfgConsts consts constss
     return Prog { getConfig = Config { getGlobalStep = gs
                                      , getDuration = dur
                                      , getConsts = allConsts
                                      , getRunFn = rfn
                                      , getBackendConfig = bcConfig
                                      }
                 , getModels = models
                 , getCouplings = couplings
                 }
  where
    checkMatch :: Eq a => a -> [a] -> (a -> a -> LinkError) -> Except LinkError ()
    checkMatch a as aErr =
      do case find (/= a) as of
           Just a' -> throwError $ aErr a a'
           Nothing -> return ()

    gSteps :: [(Integer, UnitExp Name Name)]
    gSteps = (\Prog { getConfig = Config { getGlobalStep = gs } } -> gs) <$> ps

    durs :: [Duration]
    durs = (\Prog { getConfig = Config { getDuration = dur } } -> dur) <$> ps

    constss :: [Map Identifier (Integer, UnitExp Name Name)]
    constss = (\Prog { getConfig = Config { getConsts = consts } } -> consts) <$> ps

    linkCfgConsts
      :: Map Identifier (Integer, UnitExp Name Name)
      -> [Map Identifier (Integer, UnitExp Name Name)]
      -> Except LinkError (Map Identifier (Integer, UnitExp Name Name))
    linkCfgConsts pConsts psConsts =
      do let constKeys = Map.keysSet pConsts
             restKeys = Map.keysSet <$> psConsts
             commonIdents = Set.toList $ foldr Set.intersection constKeys restKeys
         if null commonIdents then
           return $ foldr Map.union pConsts psConsts
         else
           do sequence_ $ checkConst <$> firstConstDefs commonIdents (pConsts:psConsts) <*> psConsts
              return $ foldr Map.union pConsts psConsts
      where
        findFirstDef :: Ord k => k -> [Map k a] -> Maybe a
        findFirstDef k maps =
          case catMaybes $ Map.lookup k <$> maps of
            []  -> Nothing
            a:_ -> Just a

        firstConstDefs :: [Identifier]
                       -> [Map Identifier (Integer, UnitExp Name Name)]
                       -> [(Identifier, (Integer, UnitExp Name Name))]
        firstConstDefs idents cs = zip idents $ fromJust . (`findFirstDef` cs) <$> idents

        checkConst
          :: (Identifier, (Integer, UnitExp Name Name))
          -> Map Identifier (Integer, UnitExp Name Name)
          -> Except LinkError ()
        checkConst (ident, (expectedVal, expectedUnit)) consts =
          do case Map.lookup ident consts of
               Nothing -> return ()
               Just (val, unit) ->
                 if expectedVal /= val then
                   throwError $ MismatchedConstVal ident expectedVal val
                 else when (expectedUnit /= unit) (throwError $ MismatchedConstUnit ident expectedUnit unit)


    linkModels :: [Model] -> Except LinkError Model
    linkModels _ = throwError NYI
