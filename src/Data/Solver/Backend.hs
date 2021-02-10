{-|
Module      : Solver.Backend
Description : backend-related stuff
Copyright   : (c) Galois, Inc. 2020
License     : N/A
Maintainer  : chiw@galois.com and cphifer@galois.com
Stability   : experimental
Portability : N/A
-}

module Data.Solver.Backend
  (
  BackendConfig(..)
  , Solver(..), Preconditioner(..), SolvingTechnique (..)
  , Ddt(..), DerivKind(..), NumericalScheme (..)


  ) where

import Data.Link.Identifier

data BackendConfig = Su2 {
  format ::Identifier,
  time :: Identifier,
  plotting :: Identifier
  }
  | OpenFoam
  deriving (Show)

data Solver = PCG
  deriving (Show)
data Preconditioner = DIC
  deriving (Show)
data SolvingTechnique =
  SolvingTechnique{
    solver :: Solver,
    preconditioner :: Preconditioner,
    tolerance :: Integer,
    relTol :: Integer
  }
  deriving (Show)

data Ddt = Euler
  deriving (Show)
data DerivKind = GaussLinear
  | Gauss | Linear | Orthogonal
  | GaussLinearOrthongonal
  deriving (Show)
data NumericalScheme =
  NumericalScheme {
    ddt :: Ddt,
    grad :: DerivKind,
    laplacian :: DerivKind,
    interpolation::DerivKind,
    snGrad :: DerivKind
  }
  deriving (Show)
