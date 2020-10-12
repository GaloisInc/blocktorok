{-|
Module      : Language.AST
Description : The LINK AST
Copyright   : (c) Galois, Inc. 2020
License     : N/A
Maintainer  : chiw@galois.co
Stability   : experimental
Portability : N/A

This module exports the abstract representation of the LINK concrete syntax. It
is the target of the parser defined in @Parser.y@.
-}

module Language.AST where

import Math
import Physics.Model

-- | A LINK program is a sequence of @Stmt@s
newtype Decl = DStmts [Stmt]

instance Show Decl where
  show (DStmts es) = "\n" ++ concatMap show es ++ " \n\n "

-- | Type representing a top-level statement, which is (currently) an equation
--   or a model specification
data Stmt = Equation Exp Exp Space -- ^ An equation in a particular domain
          | Box Term Model -- ^ A model labeled by a @Term@

instance Show Stmt where
  show (Equation e1 e2 e3) = "\n" ++ show e1 ++ "=" ++ show e2 ++ " in "++ show e3
  show (Box name m)   =  "\n Model " ++ show name ++ ":{" ++ show m ++ "}"
