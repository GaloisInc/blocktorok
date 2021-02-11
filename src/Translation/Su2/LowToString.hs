{-|
Module      : Translation.Su2.LowToStrings
Description :  translation from LINK AST to backend
Copyright   : Galois, Inc. 2021
License     : N/A
Maintainer  : chiw@galois.com
Stability   : Experimental
Portability : N/A

This module defines the parser for the LINK language, using Parsec.
-}

module Translation.Su2.LowToString
  (  lowToString
  ) where

import Data.Link.AST
import Data.Solver.Backend
import Data.Link.Identifier
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List
import Translation.Su2.Low

-- covert lower ir base expressions into strings
baseToString  :: Base -> String
baseToString (Int n) =  show(n)
baseToString (Name n) = n
baseToString (Neg n) = "-"++ (baseToString n)
baseToString (Pair es) = "("++intercalate "," (map baseToString es)++")"

-- covert lower ir statements into strings
stmtToString :: Stmt  -> String
stmtToString lowexp = case lowexp of
  Constructor a es -> (baseToString a) ++ "{\n\t" ++(intercalate ",\n\t" (map stmtToString es))++"\n\t}"
  Equality a b -> (baseToString a) ++ "="++(baseToString b)

-- covert lower ir to strings
lowToString :: LowIR  -> [String]
lowToString (Program es) = (map stmtToString es)
