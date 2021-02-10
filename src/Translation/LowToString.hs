{-|
Module      : Translation.Translate
Description :  translation from LINK AST to backend
Copyright   : Galois, Inc. 2021
License     : N/A
Maintainer  : chiw@galois.com
Stability   : Experimental
Portability : N/A

This module defines the parser for the LINK language, using Parsec.
-}

module Translation.LowToString
  (  lowToString
  ) where

import Data.Link.AST
import Data.Solver.Backend
import Data.Link.Identifier
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List
import Translation.Low

baseToString (Int n) =  show(n)
baseToString (Name n) = n
stmtToString lowexp = case lowexp of
  Constructor a es -> (baseToString a) ++ "{" ++(intercalate "," (map stmtToString es))++"}"
  Equality a b -> (baseToString a) ++ "="++(baseToString b)
  Incomplete -> "Should raise an error"
lowToString (Program es) = intercalate "\n\n" (map stmtToString es)
