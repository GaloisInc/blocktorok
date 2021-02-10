{-|
Module      : Translation.HighToLow
Description :  translation from LINK AST to Lower IR
Copyright   : Galois, Inc. 2021
License     : N/A
Maintainer  : chiw@galois.com
Stability   : Experimental
Portability : N/A

This module defines the parser for the LINK language, using Parsec.
-}

module Translation.HighToLow
  ( highToLow
  ) where

import Data.Link.AST
import Data.Solver.Backend
import Data.Link.Identifier
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List
import Translation.Low

err = error  "unsupported library feature"
mkVarEq lhs rhs = Equality (Name lhs) (Name rhs)
mkVarIntEq lhs rhs =  Equality (Name lhs) (Int rhs)

------------ Config ------------
------ Backend Config ------
bcFormatToLow (Identifier id) = case id of
    "LIB_Format1" -> let
      n = Name "InputOutput"
      s1 = mkVarEq "MESH_FILENAME" "mesh_solid_rod.su2"
      s2 = mkVarEq "MESH_FORMAT" "SU2"
      s3 = mkVarEq "TABULAR_FORMAT" "CSV"
      s4 = mkVarEq "CONV_FILENAME" "history"
      s5 = mkVarEq "VOLUME_FILENAME" "flow"
      s6 = mkVarEq "SURFACE_FILENAME" "surface_flow"
      s7 = mkVarIntEq "OUTPUT_WRT_FREQ" 250
      s8 = mkVarIntEq "SCREEN_WRT_FREQ_TIME" 1
      in [Constructor n [s1, s2, s3, s4, s5, s6, s7, s8]]
    _ ->  err

bcTimeToLow (Identifier id) = case id of
    "LIB_TimeDependent1" -> let
      n = Name "TimeDependent"
      s = mkVarIntEq "INNER_ITER" 200
      in [Constructor n [s]]
    _ -> err

bcPlottingToLow (Identifier id) = case id of
    "LIB_Plotting1" ->  let
      n = Name "Plotting"
      s = Equality (Name "MARKER_PLOTTING") (Name "(left, right, top, bottom)")
      in [Constructor n [s]]
    _ -> err
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
