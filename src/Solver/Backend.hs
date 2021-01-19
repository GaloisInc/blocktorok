{-|
Module      : Solver.Backend
Description : High-level representation of physical models
Copyright   : (c) Galois, Inc. 2020
License     : N/A
Maintainer  : chiw@galois.com and cphifer@galois.com
Stability   : experimental
Portability : N/A
-}

module Solver.Backend
  ( Backend(..)
  ) where

data Solver = PCG
  deriving (Show)
data Preconditioner = DIC
  deriving (Show)
data Solvers =
  T {
    solver :: Solver,
    preconditioner :: Preconditioner,
    tolerance :: Int,
    relTol :: Int
  }
  deriving (Show)

data Ddt = Euler
  deriving (Show)
data DerivKind = Gauss | Linear | Orthogonal
  deriving (Show)
data NumericalScheme =
  N {
    ddt :: Ddt,
    grad :: DerivKind,
    laplacian :: [DerivKind],
    interpolation::DerivKind,
    snGrad :: DerivKind
  }
  deriving (Show)

-- | OpenFoam solvers and numerical schemes
data Backend =
  OpenFoam {
    getSolvers :: Solvers,
    getNumericalScheme:: NumericalScheme
    }
  deriving (Show)
