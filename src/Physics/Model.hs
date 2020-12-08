{-|
Module      : Physics.Model
Description : High-level representation of physical models
Copyright   : (c) Galois, Inc. 2020
License     : N/A
Maintainer  : cphifer@galois.com
Stability   : experimental
Portability : N/A

This module provides an interface for interacting with phsycial models. There's
a lot of potentially interesting stuff to do in terms of type-level programming
here, probably, but for now it's as simple as it can be (basically a record
designed to be extended with whatever we feel is important.)
-}

module Physics.Model
  ( Model
  , mkModel
  , getTechnique
  , getConsts
  , getVars
  , getEqs
  ) where

import Data.Map.Strict (Map)
import Data.Set (Set)

import Language.Identifier
import Math
import Solver.Technique



-- | The type of a physical model; this will be computed with and eventually
--   compiled to structures allowing easy production of backend code (e.g. SU2)
data Model =
  Model {
        getTechnique :: Technique -- ^ What solving technique should be used
        , getBoundary :: Boundary
        , getOutput :: Identifier
        , getConsts :: Map Identifier Int
        , getVars :: Set Identifier
        , getEqs :: [Equation] -- ^ The equations governing the model
        }
  deriving(Show)

-- | Construct a new @Model@ from its basic components
mkModel :: Technique -> Boundary  -> Identifier ->  Map Identifier Int -> Set Identifier -> [Equation] -> Model
mkModel = Model
