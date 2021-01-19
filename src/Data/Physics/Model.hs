{-|
Module      : Data.Physics.Model
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

module Data.Physics.Model
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

import Data.Link.Identifier
import Data.Math
import Data.Solver.Technique
import Data.Units.UnitExp

import Language.Haskell.TH.Syntax (Name)

data Boundary = Neumann Identifier
              | Dirichlet Identifier
              deriving (Show)

data PhysicsType = HeatTransfer Integer
                 | FluidFlow Integer
                deriving (Show)


-- | The type of a physical model; this will be computed with and eventually
--   compiled to structures allowing easy production of backend code (e.g. SU2)
data Model =
  Model { getInput :: Identifier
        , getOutput :: Identifier
        , getTechnique :: Technique -- ^ What solving technique should be used
        , getBoundary :: Boundary
        , getPhysicsType :: PhysicsType
        , getConsts :: Map Identifier (Integer, UnitExp Name Name)
        , getLib :: Map Identifier (Identifier, Identifier)
        , getVars :: Map Identifier (UnitExp Name Name)
        , getEqs :: [Equation] -- ^ The equations governing the model
        }
  deriving(Show)

-- | Construct a new @Model@ from its basic components
mkModel :: Identifier
        -> Identifier
        -> Technique
        -> Boundary
        -> PhysicsType
        -> Map Identifier (Integer, UnitExp Name Name)
        -> Map Identifier (Identifier, Identifier)
        -> Map Identifier (UnitExp Name Name)
        -> [Equation]
        -> Model
mkModel = Model
