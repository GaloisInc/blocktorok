module Language where

-- This example comes straight from the happy documentation

-- Declarations
data Decl
    = D_Stmts [Stmt]
instance Show Decl where
  show (D_Stmts es) = "\n"++concat(map show es )++" \n\n "

data Stmt
      = Equation Exp Exp Space
      | Model Term Solve Space
instance Show Stmt where
  show (Equation e1 e2 e3) = "\n"++show(e1) ++ "=" ++ show(e2) ++ " in "++show(e3)
  show (Model s1 s2 s3)   =  "\n Model "++show(s1) ++ ":{" ++ show(s2)++","++ show(s3) ++"}"


data Space = Omega Term
instance Show Space where
  show(Omega t) = "Ω_"++show(t)

data Solve = FEM | FVM
    deriving (Show)

data Exp
     = Divergence Exp
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
       show (NablaCross e) ="("++"∇×"++show(e)++")"
       show (NablaDot e) ="("++"∇•"++show(e)++")"
       show (NablaOuter e) ="("++"∇⊗"++show(e)++")"
       show (NablaExp e) ="("++"∇"++show(e)++")"
       show (NablaSingle) ="("++"∇"++")"
       show (Laplacian e) ="("++"△"++show(e)++")"
       show (Paran e) ="("++show(e)++")"
       show (Negation e) ="("++"-"++show(e)++")"
       show (Plus e1 e2) ="("++ show(e1)++" + "++show(e2)++")"
       show (Minus e1 e2) = "("++show(e1)++" - "++show(e2)++")"
       show (Times e1 e2) = "("++show(e1)++" * "++show(e2)++")"
       show (Div e1 e2) = "("++show(e1)++"/"++show(e2)++")"
       show (CrossProduct e1 e2) = "("++show(e1)++"×"++show(e2)++")"
       show (InnerProduct e1 e2) = "("++show(e1)++"•"++show(e2)++")"
       show (OuterProduct e1 e2) = "("++show(e1)++"⊗"++show(e2)++")"
       show (Term e) = show(e)


data Term
      = Int Int
      | Var String
instance Show Term where
             show (Int e) = show(e)
             show (Var e) = e
