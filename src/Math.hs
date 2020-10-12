module Math where


newtype Space = Omega Term
instance Show Space where
  show(Omega t) = "Ω_" ++ show t

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


data Term
      = Int Int
      | Var String
instance Show Term where
             show (Int e) = show e
             show (Var e) = e
