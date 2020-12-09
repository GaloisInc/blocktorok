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
  , getLib
  , getEqs
  , Boundary(..)
  , PhysicsType(..)
  ) where

import Data.Map.Strict (Map)
import Data.Set (Set)

import Language.Identifier
import Math
import Solver.Technique


data Boundary = Neumann Identifier
              | Dirichlet Identifier
              deriving (Show)

data PhysicsType =  HeatStructure Int
                 | FluidFlow Int
                deriving (Show)


-- | The type of a physical model; this will be computed with and eventually
--   compiled to structures allowing easy production of backend code (e.g. SU2)
data Model =
  Model {
        getInput :: Identifier
        , getOutput :: Identifier
        , getTechnique :: Technique -- ^ What solving technique should be used
        , getBoundary :: Boundary
        , getPhysicsType :: PhysicsType
        , getConsts :: Map Identifier Int
        , getLib :: Map Identifier (Identifier, Identifier)
        , getVars :: Set Identifier
        , getEqs :: [Equation] -- ^ The equations governing the model
        }
  deriving(Show)

-- | Construct a new @Model@ from its basic components
mkModel :: Identifier -> Identifier
      -> Technique -> Boundary -> PhysicsType
      ->  Map Identifier Int
      ->  Map Identifier (Identifier, Identifier)
      -> Set Identifier
      -> [Equation] -> Model
mkModel = Model
