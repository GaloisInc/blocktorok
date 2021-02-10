{-|
Module      : Translation.Su2.HighToLow
Description :  translation from LINK AST to Lower IR
Copyright   : Galois, Inc. 2021
License     : N/A
Maintainer  : chiw@galois.com
Stability   : Experimental
Portability : N/A

This module defines the parser for the LINK language, using Parsec.
-}

module Translation.Su2.HighToLow
  ( highToLow
  ) where

import Data.Link.AST
import Data.Solver.Backend
import Data.Link.Identifier
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List
import Translation.Su2.Low
import Data.Physics.Model

err = error  "unsupported library feature"
mkVarEq lhs rhs =     Equality (Name lhs) (Name rhs)
mkVarIntEq lhs rhs =  Equality (Name lhs) (Int rhs)
mkVarNIntEq lhs rhs = Equality (Name lhs) (Neg (Int rhs))
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
  in (parameters)
bcToLow (OpenFoam) =  []

configToBackendParams (Config globalstep duration consts runfn backendconfig) =
  bcToLow backendconfig

------------ Models ------------
numericalSchemesToLow (Identifier id) = case id of
    "LIB_NumericalScheme1" ->  let
      n = Name "NumericalSchemes"
      s = Equality (Name " NUM_METHOD_GRAD") (Name "GREEN_GAUSS")
      in [Constructor n [s]]
    _ ->  error ("unsupported numericalSchemes"++id)

solvingTechniqueToLow (Identifier id) = case id of
    "LIB_SolvingTechnique1" -> let
      n1 = Name "LinearSolver"
      s1 = mkVarEq "LINEAR_SOLVER" "FGMRES"
      s2 = mkVarEq "LINEAR_SOLVER_PREC" "ILU"
      s3 = mkVarEq "LINEAR_SOLVER_ERROR" "1E-15"
      s4 = mkVarIntEq "LINEAR_SOLVER_ITER" 5
      n2 = Name "Convergence"
      s5 = mkVarNIntEq "CONV_RESIDUAL_MINVAL" 19
      s6 = mkVarIntEq "CONV_STARTITER" 10
      s7 = mkVarIntEq "CONV_CAUCHY_ELEMS" 100
      s8 = mkVarEq "CONV_CAUCHY_EPS" "1E-6"
      c1 = Constructor n1 [s1, s2, s3, s4]
      c2 = Constructor n2 [s5, s6, s7, s8]
      in [c1, c2]
    _ -> error  "unsupported solving technique"

varSolveToLow (VarSolve v n t) =
  (numericalSchemesToLow t) ++ (solvingTechniqueToLow n)

modelToBackendParams(Model i o t b p c l v e varsolve) =
  varSolveToLow varsolve

------------ Program ------------
highToLow(Prog config modelmap coupling)  = let
    c = configToBackendParams config
    (Config globalstep duration consts runfn backendconfig) = config
    (RFn f arg )= runfn
    model = case Map.lookup f modelmap of
      Nothing -> err
      Just model -> model
    m = modelToBackendParams model
    in Program (c++m)
