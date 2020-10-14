{-|
Module      : Math
Description : Representations of mathematical expressions
Copyright   : (c) Galois, Inc. 2020
License     : N/A
Maintainer  : chiw@galois.com
Stability   : experimental
Portability : N/A

This module provides representations of common mathematical expressions and
operations useful for specifying physical models. This will likely be extended
with all sorts of operations, notions of units, types, etc.
-}

module Math where

-- | The type of mathematical expressions
data Exp = Divergence Exp
         | Laplacian Exp
         | NablaCross Exp
         | NablaDot Exp
         | NablaOuter Exp
         | NablaExp Exp
         | NablaSingle
         | Paran Exp
         | Negation Exp
         | Plus Exp Exp
         | Minus Exp Exp
         | Times Exp Exp
         | Div Exp Exp
         | CrossProduct Exp Exp
         | InnerProduct Exp Exp
         | OuterProduct Exp Exp
         | Term Term

instance Show Exp where
  show (NablaCross e) = "(" ++ "∇×" ++ show e ++ ")"
  show (NablaDot e) = "(" ++ "∇•" ++ show e ++ ")"
  show (NablaOuter e) = "(" ++ "∇⊗" ++ show e ++ ")"
  show (NablaExp e) = "(" ++ "∇" ++ show e ++ ")"
  show NablaSingle = "(" ++ "∇" ++ ")"
  show (Laplacian e) = "(" ++ "△" ++ show e ++ ")"
  show (Paran e) = "(" ++ show e ++ ")"
  show (Negation e) = "(" ++ "-" ++ show e ++ ")"
  show (Plus e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
  show (Minus e1 e2) = "(" ++ show e1 ++ " - " ++ show e2 ++ ")"
  show (Times e1 e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
  show (Div e1 e2) = "(" ++ show e1 ++ "/" ++ show e2 ++ ")"
  show (CrossProduct e1 e2) = "(" ++ show e1 ++ "×" ++ show e2 ++ ")"
  show (InnerProduct e1 e2) = "(" ++ show e1++"•" ++ show e2 ++ ")"
  show (OuterProduct e1 e2) = "(" ++ show e1++"⊗" ++ show e2 ++ ")"
  show (Term e) = show e

-- | Mathematical terms, which are either integers or valid variable
--   identifiers
data Term = Int Int
          | Var String

-- | Mathematical equations, which consist of a left- and right-hand side.
data Equation =
  Equation { getLHS :: Exp -- ^ The left-hand side of the equation
           , getRHS :: Exp -- ^ The right-hand side of the equation
           }

instance Show Equation where
  show eq = show (getLHS eq) ++ " = " ++ show (getRHS eq)

instance Show Term where
  show (Int e) = show e
  show (Var e) = e
