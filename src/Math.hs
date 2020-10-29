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

{-# LANGUAGE MultiParamTypeClasses #-}

module Math where

import Data.Generics.Uniplate.Direct
import Data.Set (Set)
import qualified Data.Set as Set

import Language.Identifier

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

instance Uniplate Exp where
  uniplate (Divergence e)       = plate Divergence |* e
  uniplate (Laplacian e)        = plate Laplacian |* e
  uniplate (NablaCross e)       = plate NablaCross |* e
  uniplate (NablaDot e)         = plate NablaDot |* e
  uniplate (NablaOuter e)       = plate NablaOuter |* e
  uniplate (NablaExp e)         = plate NablaExp |* e
  uniplate NablaSingle          = plate NablaSingle
  uniplate (Paran e)            = plate Paran |* e
  uniplate (Negation e)         = plate Negation |* e
  uniplate (Plus e1 e2)         = plate Plus |* e1 |* e2
  uniplate (Minus e1 e2)        = plate Minus |* e1 |* e2
  uniplate (Times e1 e2)        = plate Times |* e1 |* e2
  uniplate (Div e1 e2)          = plate Div |* e1 |* e2
  uniplate (CrossProduct e1 e2) = plate CrossProduct |* e1 |* e2
  uniplate (InnerProduct e1 e2) = plate InnerProduct |* e1 |* e2
  uniplate (OuterProduct e1 e2) = plate OuterProduct |* e1 |* e2
  uniplate (Term t)             = plate Term |- t

instance Biplate Exp Exp where
  biplate = plateSelf

-- | Given an expression, return a set containing all of its variables.
vars :: Exp -> Set Identifier
vars e = Set.fromList [Identifier x | Term (Var x) <- universe e]
