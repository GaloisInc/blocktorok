

module Solver.Example
  ( Example(..)
  ) where

import Solver.Backend

data Example =
  OpenFoam (solver,numericalScheme)
