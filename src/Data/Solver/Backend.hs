{-|
Module      : Solver.Backend
Description : Solver backend configuration
Copyright   : (c) Galois, Inc. 2020
License     : N/A
Maintainer  : chiw@galois.com and cphifer@galois.com
Stability   : experimental
Portability : N/A

This module represents the parts of LINK configuration blocks related to
settings particular to different backends.

This is not a very extensible solution, but suffices for demonstration
and testing purposes until we can unify these options under some
common structured description language.
-}

module Data.Solver.Backend
  ( BackendConfig(..)
  , DerivKind(..)
  , Ddt(..)
  , NumericalScheme (..)
  , PlotMarkers(..)
  , Preconditioner(..)
  , Solver(..)
  , SolvingTechnique (..)
  ) where

import Data.Link.Identifier (Identifier)

newtype PlotMarkers = PlotMarkers [Identifier] deriving (Show)

data BackendConfig = Su2 { format ::Identifier
                         , time :: Integer
                         , plotting :: PlotMarkers
                         }
                   | OpenFoam
                   deriving (Show)

data Solver = PCG deriving (Show)

data Preconditioner = DIC deriving (Show)

data SolvingTechnique = SolvingTechnique { solver :: Solver
                                         , preconditioner :: Preconditioner
                                         , tolerance :: Integer
                                         , relTol :: Integer
                                         } deriving (Show)

data Ddt = Euler deriving (Show)

data DerivKind = GaussLinear
               | Gauss
               | Linear
               | Orthogonal
               | GaussLinearOrthongonal
               deriving (Show)

data NumericalScheme = NumericalScheme { ddt :: Ddt
                                       , grad :: DerivKind
                                       , laplacian :: DerivKind
                                       , interpolation::DerivKind
                                       , snGrad :: DerivKind
                                       } deriving (Show)
