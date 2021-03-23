{-|
Module      : Language.Compile.SU2
Description : Translation from the LINK AST to the SU2 lower IR
Copyright   : (c) Galois, Inc. 2021
License     : N/A
Maintainer  : cphifer@galois.com
Stability   : experimental
Portability : N/A

This module provides a translation between the LINK abstract syntax and SU2
configurations.
-}

module Language.Compile.SU2
  ( compile
  ) where

import Control.Monad.Except (Except, throwError)
import Control.Monad.State (StateT, execStateT, get, lift, put)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Backends.SU2
  ( IncScheme(..)
  , GradMethod(..)
  , LinearSolver(..)
  , MathProb(..)
  , MeshFormat(..)
  , Objective(..)
  , Preconditioner(..)
  , SU2Config(..)
  , SU2RHS(..)
  , SU2Solver(..)
  , TabFormat(..)
  )
import Data.Link.AST (Config(..), Prog(..), RunFn(..))
import Data.Link.Identifier (Identifier(..))
import Data.Physics.Model (Model(..), PhysicsType(..), VarSolve(..))
import Data.Solver.Backend (BackendConfig(..), PlotMarkers(..))
import Language.Error (LinkError(..))

-- TODO: Might want this to be generic in the return type, but for now all we care about is the state
type SU2Compiler = StateT SU2Config (Except LinkError) ()

-- TODO: This function should have a _type_ specialized to the SU2 backend rather than throwing
-- an error on non-SU2 - In other words, the decision to call this or another compiler function
-- should be determined earlier. Maybe some type-level magic?
compile :: Map String SU2Config -> Prog -> Except LinkError SU2Config
compile libs (Prog (Config _ _ _ (RFn f _) (Su2 (Identifier fmt) t plot)) models _) =
  execStateT compile' $ SU2Config Map.empty
  where
    compile' :: SU2Compiler
    compile' =
      do compileFmt
         compileTime
         compilePlotting
         compileModel
         compileMagic

    compileFmt :: SU2Compiler
    compileFmt =
      if fmt /= "LIB_Format1" then
        throwError $ UnknownFormat fmt
      else
        do SU2Config cfg <- get
           let beSettings = Map.fromList $ zip ["MESH_FILENAME", "MESH_FORMAT", "TABULAR_FORMAT", "CONV_FILENAME", "VOLUME_FILENAME", "SURFACE_FILENAME", "OUTPUT_WRT_FREQ", "SCREEN_WRT_FREQ_TIME"]
                                               [Filename "mesh_solid_rod.su2", MeshFormat SU2, TabularFormat CSV, Filename "history", Filename "flow", Filename "surface_flow", Integral 250, Integral 1]
           put $ SU2Config $ Map.union beSettings cfg

    compileTime :: SU2Compiler
    compileTime =
      do SU2Config cfg <- get
         put $ SU2Config $ Map.insert "INNER_ITER" (Integral t) cfg

    compilePlotting :: SU2Compiler
    compilePlotting =
      do SU2Config cfg <- get
         let mplots = case plot of
                        PlotMarkers [] -> Nothing
                        PlotMarkers ps -> Just $ (\(Identifier s) -> s) <$> ps
         put $ SU2Config $ Map.insert "MARKER_PLOTTING" (Markers mplots) cfg

    compileModel :: SU2Compiler
    compileModel =
      do Model _ _ _ _ pType _ _ _ _ (VarSolve _ st ns)  <- lift $ lookupModel f models
         compilePType pType
         compileSolveTechnique st
         compileNumScheme ns

    compilePType :: PhysicsType -> SU2Compiler
    compilePType (HeatConduction (Identifier pParams)) =
      if pParams /= "LIB_PhysicsParameters1" then
        throwError $ UnknownPhysParams pParams
      else
        do SU2Config cfg <- get
           let pSettings = Map.fromList $ zip ["SOLVER", "MATH_PROBLEM", "RESTART_SO", "OBJECTIVE_FUNCTION", "INC_NONDIM", "SOLID_DENSITY", "SPECIFIC_HEAT_CP", "SOLID_THERMAL_CONDUCTIVITY"]
                                              [Solver Heat, MathProblem Direct, Boolean False, ObjectiveFns [TotalHeatFlux], IncompressibleScheme Dim, Floating 19300.0, Floating 130.0, Floating 318.0]
           put $ SU2Config $ Map.union pSettings cfg
    compilePType pType = throwError $ UnsupportedPhys pType

    compileSolveTechnique :: Identifier -> SU2Compiler
    compileSolveTechnique (Identifier st) =
      if st /= "LIB_SolvingTechnique1" then
        throwError $ UnknownSolvingTech st
      else
        do SU2Config cfg <- get
           let stSettings = Map.fromList $ zip ["LINEAR_SOLVER", "LINEAR_SOLVER_PREC", "LINEAR_SOLVER_ERROR","LINEAR_SOLVER_ITER", "CONV_RESIDUAL_MINVAL", "CONV_STARTITER", "CONV_CAUCY_ELEMS", "CONV_CAUCHY_EPS"]
                                               [LinearSolver FGMRes, Preconditioner ILU, Floating 1e-15, Integral 5, Integral (negate 19), Integral 10, Integral 100, Floating 1e-6]
           put $ SU2Config $ Map.union stSettings cfg

    compileNumScheme :: Identifier -> SU2Compiler
    compileNumScheme (Identifier ns) =
      if ns /= "LIB_NumericalScheme1" then
        throwError $ UnknownNumScheme ns
      else
        do SU2Config cfg <- get
           put $ SU2Config $ Map.insert "NUM_METHOD_GRAD" (GradientMehod GGauss) cfg

    compileMagic :: SU2Compiler
    compileMagic =
      do SU2Config cfg <- get
         let magic = Map.fromList $ zip ["TIME_DOMAIN", "TIME_STEP", "MAX_TIME", "TIME_ITER", "MARKER_HEATFLUX", "MARKER_PLOTTING", "SOLID_TEMPERATURE_INIT"]
                                        [Boolean True, Floating 0.005, Integral 3, Integral 600, MarkerData [("top", 0), ("bottom", 0)], Markers $ Just ["left", "right", "top", "bottom"], Floating 273.0]
         put $ SU2Config $ Map.union magic cfg

    lookupModel :: Identifier -> Map Identifier Model -> Except LinkError Model
    lookupModel ident ms =
      case Map.lookup ident ms of
        Nothing -> throwError $ NoModelWithName $ show ident
        Just m  -> return m
compile _ _ = throwError NYI
