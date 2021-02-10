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

module Translation.Translate
  ( highToLow
  , lowToString
  ) where

import Data.Link.AST
import Data.Solver.Backend
import Data.Link.Identifier
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List



data Base = Int Integer | Name String
data Stmt = Constructor Base [Stmt]
  | Equality Base Base
  | Incomplete
data Program = Program [Stmt]
err = Incomplete -- "Should raise an error"

baseToString (Int n) =  show(n)
baseToString (Name n) = n
stmtToString lowexp = case lowexp of
  Constructor a es -> (baseToString a) ++ "{" ++(intercalate "," (map stmtToString es))++"}"
  Equality a b -> (baseToString a) ++ "="++(baseToString b)
  Incomplete -> "Should raise an error"
lowToString (Program es) = intercalate "\n\n" (map stmtToString es)


------------ Config ------------
------ Backend Config ------
bcFormatToLow (Identifier id) = case id of
    "LIB_Format1" -> [err]
    _ -> [err]

bcTimeToLow (Identifier id) = case id of
    "LIB_TimeDependent1" -> let
      s = Equality (Name "INNER_ITER") (Int 200)
      n = Name "TimeDependent"
      in [Constructor n [s]]
    _ -> [err]

bcPlottingToLow (Identifier id) = case id of
    "LIB_Plotting1" ->  let
      s = Equality (Name "MARKER_PLOTTING") (Name "(left, right, top, bottom)")
      n = Name "Plotting"
      in [Constructor n [s]]
    _ -> [err]
bcToLow (Su2 format time plotting) = let
  f = bcFormatToLow  format
  t = bcTimeToLow  time
  p = bcPlottingToLow  plotting
  parameters = f ++ t++ p
  in (Program parameters)
bcToLow (OpenFoam) = Program []

configToLow (Config globalstep duration consts runfn backendconfig) =
  bcToLow backendconfig

------------ Program ------------
highToLow(Prog config models coupling)  =
    (configToLow config)
