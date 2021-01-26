{-|
Module      : Solver.Backend
Description : High-level representation of physical models
Copyright   : (c) Galois, Inc. 2020
License     : N/A
Maintainer  : chiw@galois.com and cphifer@galois.com
Stability   : experimental
Portability : N/A
-}

module Data.Solver.Backend
  (
  Solver(..), Preconditioner(..), SolvingTechnique (..)
  , Ddt(..), DerivKind(..), NumericalScheme (..)


  ) where

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
data DerivKind = Gauss | Linear | Orthogonal
  deriving (Show)
data NumericalScheme =
  NumericalScheme {
    ddt :: Ddt,
    grad :: [DerivKind],
    laplacian :: [DerivKind],
    interpolation::DerivKind,
    snGrad :: DerivKind
  }
  deriving (Show)
