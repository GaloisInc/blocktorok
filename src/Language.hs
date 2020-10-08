module Language where

import Technique
import Math
import PhysicsType
import PhysicsModel

-- Declarations
data Decl
    = D_Stmts [Stmt]
instance Show Decl where
  show (D_Stmts es) = "\n"++concat(map show es )++" \n\n "

data Stmt
      = Equation Exp Exp Space
      | Box Term Model
instance Show Stmt where
  show (Equation e1 e2 e3) = "\n"++show(e1) ++ "=" ++ show(e2) ++ " in "++show(e3)
  show (Box name m)   =  "\n Model "++show(name) ++ ":{" ++ show(m) ++"}"
