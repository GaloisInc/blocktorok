module Data.Equation where

import Data.Math as Math
import Data.LatexSyntax as Latex

data Equation =
    MathEquation Math.Equation
  | LatexEquation Latex.Equation
  deriving Show