{-|
Module      : Translation.HighToStr
Description :  translation from LINK AST to a backend-specific Lower IR
Copyright   : Galois, Inc. 2021
License     : N/A
Maintainer  : chiw@galois.com
Stability   : Experimental
Portability : N/A

This module defines the parser for the LINK language, using Parsec.
-}

module Translation.HighToStr
  ( highToStr
  ) where

import Translation.Su2.HighToLow ( highToLow )
import Translation.Su2.LowToString ( lowToString )
import Data.Link.AST
import Data.Solver.Backend
import Data.Link.Identifier
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List
import Translation.Su2.Low
import Data.Physics.Model

highToStr e = let
  Prog config modelmap coupling = e
  (Config globalstep duration consts runfn backendconfig) = config
  in case backendconfig of
    Su2 _ _ _ -> let
        low = highToLow e
        in lowToString low
    OpenFoam -> error "OpenFoam backend incomplete"
