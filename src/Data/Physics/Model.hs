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
  , VarSolve(..)
  , BoundaryType(..)
  , BoundaryField(..)
  , Boundary(..)
  , PhysicsType(..)
  ) where

import Data.Map.Strict (Map)
import Data.Set (Set)

import Data.Link.Identifier
import Data.Math
import Data.Solver.Technique
import Data.Units.UnitExp
import Language.Haskell.TH.Syntax (Name)
import qualified Data.Equation as Eqn

-- An ugly function I added. Forgive me.
mkIndent lhs rhs = "\n\t"++show(lhs)++" : "++show(rhs)

data BoundaryType = Neumann | Dirichlet
              deriving (Show)

data BoundaryField =  BoundaryField Identifier  BoundaryType Integer
              deriving (Show)

data Boundary =  T BoundaryType Identifier | F [BoundaryField]
              deriving (Show)

data PhysicsType =  HeatTransfer Identifier
                 | FluidFlow Identifier
                 | HeatConduction Identifier
                deriving (Show)
data VarSolve = VarSolve
  Identifier   --  ^ Variable we are solving for
  Identifier    -- , solving technique
  Identifier   ---, and numerical scheme
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
        , getConsts :: Map Identifier (Integer, UnitExp Name Name)
        , getLib :: Map Identifier (Identifier, Identifier)
        , getVars :: Map Identifier (UnitExp Name Name)
        , getEqs :: [Eqn.Equation] -- ^ The equations governing the model
        , getSolve :: VarSolve
        }
instance Show Model where
      show (Model i o t b p c l v e s) =
        (mkIndent "input" i)
        ++ (mkIndent "output" o)
        ++ (mkIndent "Technique" t)
        ++ (mkIndent "Boundary" b)
        ++ (mkIndent "PhysicsType" p)
        ++ (mkIndent "constants" c)
        ++ (mkIndent "libraries" l)
        ++ (mkIndent "variables" v)
        ++ (mkIndent "equations" e)
        ++ (mkIndent "solvingvariables" s)

-- | Construct a new @Model@ from its basic components
mkModel :: Identifier -> Identifier
      -> Technique -> Boundary -> PhysicsType
      ->  Map Identifier (Integer, UnitExp Name Name)
      ->  Map Identifier (Identifier, Identifier)
      ->  Map Identifier (UnitExp Name Name)
      -> [Eqn.Equation]
      -> VarSolve
      -> Model
mkModel = Model
