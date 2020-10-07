module Language where

-- This example comes straight from the happy documentation

-- Declarations
data Decl
    = D_Stmts [Stmt]
instance Show Decl where
  show (D_Stmts es) = "\n"++concat(map show es )++" \n\n "

data Stmt
      = Equation Exp Exp
instance Show Stmt where
  show (Equation e1 e2) = show(e1) ++ "=" ++ show(e2)

data Exp
     = Divergence Exp
     | Laplacian Exp
     | Nabla Exp
     | Paran Exp
     | Negation Exp
     | Plus Exp Exp
     | Minus Exp Exp
     | Times Exp Exp
     | InnerProduct Exp Exp
     | Div Exp Exp
     | Term Term
     deriving Show

data Term
      = Int Int
      | Var String
      deriving Show
