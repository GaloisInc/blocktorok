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
                  | SJacobi
                  | SILU
                  | SLUSGS
                  | SLinelet
instance Render LinearSolver where
  render FGMRes = "FGMRES"
  render RestartFGMRes = "RESTARTED_FGMRES"
  render BCGStab = "BCGSTAB"
  render SJacobi = "SMOOTHER_JACOBI"
  render SILU = "SMOOTHER_ILU"
  render SLUSGS = "SMOOTHER_LUSGS"
  render SLinelet = "SMOOTHER_LINELET"

data Preconditioner = ILU
                    | LU_SGS
                    | Linelet
                    | Jacobi
instance Render Preconditioner where
  render ILU = "ILU"
  render LU_SGS = "LU_SGS"
  render Linelet = "LINELET"
  render Jacobi = "Jacobi"

data Stiffness = InvVol
               | WallDist
               | ConstStiff
instance Render Stiffness where
  render InvVol = "INVERSE_VOLUME"
  render WallDist = "WALL_DISTANCE"
  render ConstStiff = "CONSTANT_STIFFNESS"

data TimeDiscre = EulerImp
                | RKExp
                | EulerExp
                | RK4Exp
                | AderDG
instance Render TimeDiscre where
  render EulerImp = "EULER_IMPLICIT"
  render RKExp = "RUNGE-KUTTA_EXPLICIT"
  render EulerExp = "EULER_EXPLICIT"
  render RK4Exp = "CLASSICAL_RK4_EXPLICIT"
  render AderDG = "ADER_DG"

data TabFormat = TECPLOT
               | CSV
instance Render TabFormat where
  render TECPLOT = "TECPLOT"
  render CSV = "CSV"

-- TODO: Given that the order of these things in SU2 configs doesn't matter, this should probably just be a map
-- Problem: Need a type to capture the things valid on the RHS of config lines. Not too hard but could make it
-- difficult to extend/make changes later.
data SU2Config =
  SU2Config { getSolver         :: SU2Solver
            , isRestart         :: SU2Bool
            , getObjectiveFn    :: Objective -- TODO: Support multiple
            , getObjectiveWt    :: SU2Double -- TODO: Support multiple
            , isTimeDomain      :: SU2Bool
            , getTimeStep       :: SU2Double -- TODO: Units?
            , getMaxTime        :: SU2Double -- TODO: Units?
            , getInternalIter   :: SU2Integer
            , getTimeIter       :: SU2Integer
            , getMarkerIso      :: [(String, SU2Double)] -- TODO: Reader for zone markers?
            , getMarkerHeatFlux :: [(String, SU2Double)]
            , getMarkerPlot     :: [String]
            , getMarkerMonitor  :: Maybe [String]
            , getIncScheme      :: IncScheme
            , getSolidTempInit  :: SU2Double -- TODO: Units?
            , getSolidDensity   :: SU2Double -- TODO: Units?
            , getSpecHeat       :: SU2Double -- TODO: Units?
            , getSolidThermCond :: SU2Double -- TODO: Units?
            , getGradMethod     :: GradMethod
            , getCFLNum         :: SU2Double
            , isCFLAdapt        :: SU2Bool
            , getCFLAdapParam   :: (SU2Double, SU2Double, SU2Double, SU2Double)
            , getRKCoeff        :: (SU2Double, SU2Double, SU2Double)
            , getLinSolver      :: LinearSolver
            , getPreconditioner :: Preconditioner
            , getFillLevel      :: SU2Integer
            , getLinSolvIter    :: SU2Integer
            , getTimeDiscHeat   :: TimeDiscre -- TODO: This can't be everything this type offers. Should mitigate.
            , getConvResMin     :: SU2Integer -- TODO: This might need to be a float
            , getConvStartIter  :: SU2Integer
            , getMeshfile       :: String
            , getMeshOutFile    :: String
            , getTabFormat      :: TabFormat
            , getSolnFile       :: String
            , getSolnAdjFile    :: String
            , getConvFile       :: String
            , getBreakdownFile  :: String
            , getRestartFile    :: String
            , getRestartAdjFile :: String
            , getVolFile        :: String
            , getVolAdjFile     :: String
            , getVolObjFile     :: String
            , getGradFile       :: String
            , getSurfaceFile    :: String
            , getSurfaceAdjFile :: String
            , getWrtSolFreq     :: SU2Integer
            , getWrtConFreq     :: SU2Integer
            , getDefLinSolver   :: LinearSolver -- TODO: This can't be everything this type offers. Should mitigate.
            , getDefLinSolvIter :: SU2Integer
            , getDefNonlinIter  :: SU2Integer
            , shouldDefOut      :: SU2Bool
            , getStiffType      :: Stiffness
            , shouldVizVolDef   :: SU2Bool
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
                ++ "LINEAR_SOLVER= "              ++ render (getLinSolver su2conf)                                                                            ++   "\n"
                ++ "LINEAR_SOLVER_PREC= "         ++ render (getPreconditioner su2conf)                                                                       ++   "\n"
                ++ "LINEAR_SOLVER_ILU_FILL_IN= "  ++ render (getFillLevel su2conf)                                                                            ++   "\n"
                ++ "LINEAR_SOLVER_ERROR= "                                                                                                                    ++   "\n"
                ++ "LINEAR_SOLVER_ITER= "         ++ render (getLinSolvIter su2conf)                                                                          ++   "\n"
                ++ "TIME_DISCRE_HEAT= "           ++ render (getTimeDiscHeat su2conf)                                                                         ++   "\n"
                ++ "CONV_RESIDUAL_MINVAL= "       ++ render (getConvResMin su2conf)                                                                           ++   "\n"
                ++ "CONV_STARTITER= "             ++ render (getConvStartIter su2conf)                                                                        ++   "\n"
                ++ "CONV_CAUCHY_EPS= "                                                                                                                        ++   "\n"
                ++ "MESH_FILENAME= "              ++ getMeshfile su2conf                                                                                      ++   "\n" -- TODO: Get this from clargs
                ++ "MESH_FORMAT= SU2\n"
                ++ "MESH_OUT_FILENAME= "          ++ getMeshOutFile su2conf                                                                                   ++   "\n" -- TODO: Get this from clargs
                ++ "TABULAR_FORMAT= "             ++ render (getTabFormat su2conf)                                                                            ++   "\n"
                ++ "SOLUTION_FILENAME= "          ++ getSolnFile su2conf                                                                                      ++   "\n"
                ++ "SOLUTION_ADJ_FILENAME= "      ++ getSolnAdjFile su2conf                                                                                   ++   "\n"
                ++ "CONV_FILENAME= "              ++ getConvFile su2conf                                                                                      ++   "\n"
                ++ "BREAKDOWN_FILENAME= "         ++ getBreakdownFile su2conf                                                                                 ++   "\n"
                ++ "RESTART_FILENAME= "           ++ getRestartFile su2conf                                                                                   ++   "\n"
                ++ "RESTART_ADJ_FILENAME= "       ++ getRestartAdjFile su2conf                                                                                ++   "\n"
                ++ "VOLUME_FILENAME= "            ++ getVolFile su2conf                                                                                       ++   "\n"
                ++ "VOLUME_ADJ_FILENAME= "        ++ getVolAdjFile su2conf                                                                                    ++   "\n"
                ++ "VALUE_OBJFUNC_FILENAME= "     ++ getVolObjFile su2conf                                                                                    ++   "\n"
                ++ "GRAD_OBJFUNC_FILENAME= "      ++ getGradFile su2conf                                                                                      ++   "\n"
                ++ "SURFACE_FILENAME= "           ++ getSurfaceFile su2conf                                                                                   ++   "\n"
                ++ "SURFACE_ADJ_FILENAME= "       ++ getSurfaceAdjFile su2conf                                                                                ++   "\n"
                ++ "WRT_SOL_FREQ= "               ++ render (getWrtSolFreq su2conf)                                                                           ++   "\n"
                ++ "WRT_CON_FREQ= "               ++ render (getWrtConFreq su2conf)                                                                           ++   "\n"
                ++ "DEFORM_LINEAR_SOLVER= "       ++ render (getDefLinSolver su2conf)                                                                         ++   "\n"
                ++ "DEFORM_LINEAR_SOLVER_ITER= "  ++ render (getDefLinSolvIter su2conf)                                                                       ++   "\n"
                ++ "DEFORM_NONLINEAR_ITER= "      ++ render (getDefNonlinIter su2conf)                                                                        ++   "\n"
                ++ "DEFORM_CONSOLE_OUTPUT= "      ++ render (shouldDefOut su2conf)                                                                            ++   "\n"
                ++ "DEFORM_STIFFNESS_TYPE= "      ++ render (getStiffType su2conf)                                                                            ++   "\n"
                ++ "VISUALIZE_VOLUME_DEF= "       ++ render (shouldVizVolDef su2conf)                                                                         ++   "\n"