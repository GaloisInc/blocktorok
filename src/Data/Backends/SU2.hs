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
            }
instance Render SU2Config where
  render su2conf = "SOLVER= " ++ render (getSolver su2conf) ++ "\n"
                ++ "RESTART_SOL= " ++ render (isRestart su2conf) ++ "\n"
                ++ "OBJECTIVE_FUNCTION= " ++ render (getObjectiveFn su2conf) ++ "\n"
                ++ "OBJECTIVE_WEIGHT= " ++ render (getObjectiveWt su2conf) ++ "\n"
                ++ "TIME_DOMAIN= " ++ render (isTimeDomain su2conf) ++ "\n"
                ++ "TIME_STEP= " ++ render (getTimeStep su2conf) ++ "\n"
                ++ "MAX_TIME= " ++ render (getMaxTime su2conf) ++ "\n"
                ++ "INNER_ITER= " ++ render (getInternalIter su2conf) ++ "\n"
                ++ "TIME_ITER= " ++ render (getTimeIter su2conf) ++ "\n"
                ++ "MARKER_ISOTHERMAL= ( " ++ intercalate ", " ((\(name, value) -> name ++ ", " ++ render value) <$> getMarkerIso su2conf) ++ " )\n"
                ++ "MARKER_HEATFLUX= (" ++ intercalate ", " ((\(name, value) -> name ++ ", " ++ render value) <$> getMarkerHeatFlux su2conf) ++ " )\n"
                ++ "MARKER_PLOTTING= (" ++ intercalate ", " (getMarkerPlot su2conf) ++ " )\n"
