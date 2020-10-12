module Language where

import Math
import Physics.Model

-- Declarations
newtype Decl = DStmts [Stmt]
instance Show Decl where
  show (DStmts es) = "\n" ++ concatMap show es ++ " \n\n "

data Stmt
      = Equation Exp Exp Space
      | Box Term Model
instance Show Stmt where
  show (Equation e1 e2 e3) = "\n" ++ show e1 ++ "=" ++ show e2 ++ " in "++ show e3
  show (Box name m)   =  "\n Model " ++ show name ++ ":{" ++ show m ++ "}"
