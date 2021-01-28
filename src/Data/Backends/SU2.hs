{-|
Module      : Data.Backends.SU2
Description : Internal representation of SU2 configuration scripts
Copyright   : (c) Galois, Inc. 2021
License     : N/A
Maintainer  : cphifer@galois.com
Stability   : experimental
Portability : N/A
-}

module Data.Backends.SU2
  ( SU2Config(..)
  ) where

import Data.Class.Render
import Data.List (intercalate)

newtype SU2Bool = SU2Bool Bool
newtype SU2Double = SU2Double Double
newtype SU2Integer = SU2Integer Integer

instance Render SU2Bool where
  render (SU2Bool True)  = "YES"
  render (SU2Bool False) = "NO"

instance Render SU2Double where
  render (SU2Double x) = show x

instance Render SU2Integer where
  render (SU2Integer i) = show i

data SU2Solver = Euler
               | NS
               | Wave
               | Heat
               | ElasticityFEM
               | Poisson
instance Render SU2Solver where
  render Euler = "EULER"
  render NS = "NAVIER_STOKES"
  render Wave = "WAVE_EQUATION"
  render Heat = "HEAT_EQUATION"
  render ElasticityFEM = "FEM_ELASTICITY"
  render Poisson = "POISSON_EQUATION"

data Objective = Drag
               | Lift
               | Sideforce
               | XMoment
               | YMoment
               | ZMoment
               | Efficiency
               | EquivArea
               | NearPressure
               | XForce
               | YForce
               | ZForce
               | Thrust
               | Torque
               | TotalHeatFlux
               | MaxHeatFlux
               | InvDesPressure
               | InvDesHeatFlux
               | SurfTotPressure
               | SurfMassFlow
               | SurfStatPressure
               | SurfMach
instance Render Objective where
  render Drag = "DRAG"
  render Lift = "LIFT"
  render Sideforce = "SIDEFORCE"
  render XMoment = "MOMENT_X"
  render YMoment = "MOMENT_Y"
  render ZMoment = "MOMENT_Z"
  render Efficiency = "EFFICIENCY"
  render EquivArea = "EQUIVALENT_AREA"
  render NearPressure = "NEARFIELD_PRESSURE"
  render XForce = "FORCE_X"
  render YForce = "FORCE_Y"
  render ZForce = "FORCE_Z"
  render Thrust = "THRUST"
  render Torque = "TORQUE"
  render TotalHeatFlux = "TOTAL_HEATFLUX"
  render MaxHeatFlux = "MAXIMUM_HEATFLUX"
  render InvDesPressure = "INVERSE_DESIGN_PRESSURE"
  render InvDesHeatFlux = "INVERSE_DESIGN_HEATFLUX"
  render SurfTotPressure = "SURFACE_TOTAL_PRESSURE"
  render SurfMassFlow = "SURFACE_MASSFLOW"
  render SurfStatPressure = "SURFACE_STATIC_PRESSURE"
  render SurfMach = "SURFACE_MACH"

data IncScheme = InitValues
               | RefValues
               | Dim
instance Render IncScheme where
  render InitValues = "INITIAL_VALUES"
  render RefValues = "REFERENCE_VALUES"
  render Dim = "DIMENSIONAL"

data GradMethod = GGauss
                | WLS
instance Render GradMethod where
  render GGauss = "GREEN_GAUSS"
  render WLS = "WEIGHTED_LEAST_SQUARES"

data LinearSolver = FGMRes
                  | RestartFGMRes
                  | BCGStab
instance Render LinearSolver where
  render FGMRes = "FGMRES"
  render RestartFGMRes = "RESTARTED_FGMRES"
  render BCGStab = "BCGSTAB"

data Stiffness = InvVol
               | WallDist
               | ConstStiff
instance Render Stiffness where
  render InvVol = "INVERSE_VOLUME"
  render WallDist = "WALL_DISTANCE"
  render ConstStiff = "CONSTANT_STIFFNESS"

data SU2Config =
  SU2Config { getSolver :: SU2Solver
            , isRestart :: SU2Bool
            , getObjectiveFn :: Objective -- TODO: Support multiple
            , getObjectiveWt :: SU2Double -- TODO: Support multiple
            , isTimeDomain :: SU2Bool
            , getTimeStep :: SU2Double -- TODO: Units?
            , getMaxTime :: SU2Double -- TODO: Units?
            , getInternalIter :: SU2Integer
            , getTimeIter :: SU2Integer
            , getMarkerIso :: [(String, SU2Double)] -- TODO: Reader for zone markers?
            , getMarkerHeatFlux :: [(String, SU2Double)]
            , getMarkerPlot :: [String]
            , getMarkerMonitor :: Maybe [String]
            , getIncScheme :: IncScheme
            , getSolidTempInit :: SU2Double -- TODO: Units?
            , getSolidDensity :: SU2Double -- TODO: Units?
            , getSpecHeat :: SU2Double -- TODO: Units?
            , getSolidThermCond :: SU2Double -- TODO: Units?
            , getGradMethod :: GradMethod
            , getCFLNum :: SU2Double
            , isCFLAdapt :: SU2Bool
            , getCFLAdapParam :: (SU2Double, SU2Double, SU2Double, SU2Double)
            , getRKCoeff :: (SU2Double, SU2Double, SU2Double)
            , getDefLinSolver :: LinearSolver
            , getStiffType :: Stiffness
            }
instance Render SU2Config where
  render su2conf = "SOLVER= "                     ++ render (getSolver su2conf)                                                                               ++   "\n"
                ++ "RESTART_SOL= "                ++ render (isRestart su2conf)                                                                               ++   "\n"
                ++ "OBJECTIVE_FUNCTION= "         ++ render (getObjectiveFn su2conf)                                                                          ++   "\n"
                ++ "OBJECTIVE_WEIGHT= "           ++ render (getObjectiveWt su2conf)                                                                          ++   "\n"
                ++ "TIME_DOMAIN= "                ++ render (isTimeDomain su2conf)                                                                            ++   "\n"
                ++ "TIME_STEP= "                  ++ render (getTimeStep su2conf)                                                                             ++   "\n"
                ++ "MAX_TIME= "                   ++ render (getMaxTime su2conf)                                                                              ++   "\n"
                ++ "INNER_ITER= "                 ++ render (getInternalIter su2conf)                                                                         ++   "\n"
                ++ "TIME_ITER= "                  ++ render (getTimeIter su2conf)                                                                             ++   "\n"
                ++ "MARKER_ISOTHERMAL= ( "        ++ intercalate ", " ((\(name, value) -> name ++ ", " ++ render value) <$> getMarkerIso su2conf)             ++ " )\n"
                ++ "MARKER_HEATFLUX= ( "          ++ intercalate ", " ((\(name, value) -> name ++ ", " ++ render value) <$> getMarkerHeatFlux su2conf)        ++ " )\n"
                ++ "MARKER_PLOTTING= ( "          ++ intercalate ", " (getMarkerPlot su2conf)                                                                 ++ " )\n"
                ++ "MARKER_MONITORING= ( "        ++ case getMarkerMonitor su2conf of { Nothing -> "NONE"; Just markers -> intercalate ", " markers }         ++ " )\n"
                ++ "INC_NONDIM= "                 ++ render (getIncScheme su2conf)                                                                            ++   "\n"
                ++ "SOLID_TEMPERATURE_INIT= "     ++ render (getSolidTempInit su2conf)                                                                        ++   "\n"
                ++ "SOLID_DENSITY= "              ++ render (getSolidDensity su2conf)                                                                         ++   "\n"
                ++ "SPECIFIC_HEAT_CP= "           ++ render (getSpecHeat su2conf)                                                                             ++   "\n"
                ++ "SOLID_THERMAL_CONDUCTIVITY= " ++ render (getSolidThermCond su2conf)                                                                       ++   "\n"
                ++ "NUM_METHOD_GRAD= "            ++ render (getGradMethod su2conf)                                                                           ++   "\n"
                ++ "CFL_NUMBER= "                 ++ render (getCFLNum su2conf)                                                                               ++   "\n"
                ++ "CFL_ADAPT= "                  ++ render (isCFLAdapt su2conf)                                                                              ++   "\n"
                ++ "CFL_ADAPT_PARAM= ( "          ++ intercalate ", " (let (fd, fu, minV, maxV) = getCFLAdapParam su2conf in render <$> [fd, fu, minV, maxV]) ++ " )\n"
                ++ "RK_ALPHA_COEFF= "             ++ intercalate ", " (let (x, y, z) = getRKCoeff su2conf in render <$> [x, y, z])                            ++   "\n"
                ++ "LINEAR_SOLVER= "                                                                                                                          ++   "\n"
                ++ "LINEAR_SOLVER_PREC= "                                                                                                                     ++   "\n"
                ++ "LINEAR_SOLVER_ILU_FILL_IN= "                                                                                                              ++   "\n"
                ++ "LINEAR_SOLVER_ERROR= "                                                                                                                    ++   "\n"
                ++ "LINEAR_SOLVER_ITER= "                                                                                                                     ++   "\n"
                ++ "TIME_DISCRE_HEAT= "                                                                                                                       ++   "\n"
                ++ "CONV_RESIDUAL_MINVAL= "                                                                                                                   ++   "\n"
                ++ "CONV_STARTITER= "                                                                                                                         ++   "\n"
                ++ "CONV_CAUCHY_EPS= "                                                                                                                        ++   "\n"
                ++ "MESH_FILENAME= "                                                                                                                          ++   "\n" -- TODO: Get this from clargs
                ++ "MESH_FORMAT= SU2\n"
                ++ "MESH_OUT_FILENAME= "                                                                                                                      ++   "\n" -- TODO: Get this from clargs
                ++ "TABULAR_FORMAT= "                                                                                                                         ++   "\n"
                ++ "SOLUTION_FILENAME= "                                                                                                                      ++   "\n"
                ++ "SOLUTION_ADJ_FILENAME= "                                                                                                                  ++   "\n"
                ++ "CONV_FILENAME= "                                                                                                                          ++   "\n"
                ++ "BREAKDOWN_FILENAME= "                                                                                                                     ++   "\n"
                ++ "RESTART_FILENAME= "                                                                                                                       ++   "\n"
                ++ "RESTART_ADJ_FILENAME= "                                                                                                                   ++   "\n"
                ++ "VOLUME_FILENAME= "                                                                                                                        ++   "\n"
                ++ "VOLUME_ADJ_FILENAME= "                                                                                                                    ++   "\n"
                ++ "VALUE_OBJFUNC_FILENAME= "                                                                                                                 ++   "\n"
                ++ "GRAD_OBJFUNC_FILENAME= "                                                                                                                  ++   "\n"
                ++ "SURFACE_FILENAME= "                                                                                                                       ++   "\n"
                ++ "SURFACE_ADJ_FILENAME= "                                                                                                                   ++   "\n"
                ++ "WRT_SOL_FREQ= "                                                                                                                           ++   "\n"
                ++ "WRT_CON_FREQ= "                                                                                                                           ++   "\n"
                ++ "DEFORM_LINEAR_SOLVER= "       ++ render (getDefLinSolver su2conf)                                                                         ++   "\n"
                ++ "DEFORM_LINEAR_SOLVER_ITER= "                                                                                                              ++   "\n"
                ++ "DEFORM_NONLINEAR_ITER= "                                                                                                                  ++   "\n"
                ++ "DEFORM_CONSOLE_OUTPUT= "                                                                                                                  ++   "\n"
                ++ "DEFORM_STIFFNESS_TYPE= "      ++ render (getStiffType su2conf)                                                                            ++   "\n"
                ++ "VISUALIZE_VOLUME_DEF= "                                                                                                                   ++   "\n"