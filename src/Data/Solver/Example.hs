

module Data.Solver.Example
  (
  ) where

import Data.Solver.Backend

t =   SolvingTechnique {
    solver = PCG,
    preconditioner = DIC,
    tolerance = 1,
    relTol = 0
  }
n =   NumericalScheme  {
    ddt = Euler,
    grad = [Gauss, Linear],
    laplacian = [Gauss, Linear, Orthogonal],
    interpolation = Linear,
    snGrad = Orthogonal
  }
