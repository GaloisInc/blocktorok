{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Data.Backends.SU2
Description : Internal representation of SU2 configuration scripts
Copyright   : (c) Galois, Inc. 2021
License     : N/A
Maintainer  : cphifer@galois.com
Stability   : experimental
Portability : N/A

This module defines a representation of SU2 configuration scripts. Given that
SU2 operates on what are essentially simple key-value pairings, this
representation is as straightforward as possible: A map from string
identifiers to a custom algebraic type capturing legal RHS values in SU2.

The types presented here do not fully capture all SU2 options - it is merely
enough to compile some of the simpler toy problems
-}

module Data.Backends.SU2
  ( GradMethod(..)
  , IncScheme(..)
  , LinearSolver(..)
  , MathProb(..)
  , MeshFormat(..)
  , Objective(..)
  , Preconditioner(..)
  , SU2Config(..)
  , SU2RHS(..)
  , SU2Solver(..)
  , TabFormat(..)
  ) where

import Data.Aeson (FromJSON, Value(..), parseJSON, withText)

import Data.Class.Render (Render, render)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Scientific (floatingOrInteger)

import Data.Text (unpack)

data SU2Solver = Euler
               | NS
               | Wave
               | Heat
               | ElasticityFEM
               | Poisson
instance Render SU2Solver where
  render Euler         = "EULER"
  render NS            = "NAVIER_STOKES"
  render Wave          = "WAVE_EQUATION"
  render Heat          = "HEAT_EQUATION"
  render ElasticityFEM = "FEM_ELASTICITY"
  render Poisson       = "POISSON_EQUATION"

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
  render Drag             = "DRAG"
  render Lift             = "LIFT"
  render Sideforce        = "SIDEFORCE"
  render XMoment          = "MOMENT_X"
  render YMoment          = "MOMENT_Y"
  render ZMoment          = "MOMENT_Z"
  render Efficiency       = "EFFICIENCY"
  render EquivArea        = "EQUIVALENT_AREA"
  render NearPressure     = "NEARFIELD_PRESSURE"
  render XForce           = "FORCE_X"
  render YForce           = "FORCE_Y"
  render ZForce           = "FORCE_Z"
  render Thrust           = "THRUST"
  render Torque           = "TORQUE"
  render TotalHeatFlux    = "TOTAL_HEATFLUX"
  render MaxHeatFlux      = "MAXIMUM_HEATFLUX"
  render InvDesPressure   = "INVERSE_DESIGN_PRESSURE"
  render InvDesHeatFlux   = "INVERSE_DESIGN_HEATFLUX"
  render SurfTotPressure  = "SURFACE_TOTAL_PRESSURE"
  render SurfMassFlow     = "SURFACE_MASSFLOW"
  render SurfStatPressure = "SURFACE_STATIC_PRESSURE"
  render SurfMach         = "SURFACE_MACH"

instance FromJSON Objective where
  parseJSON = withText "Objective" $ \case
    "Drag" -> pure Drag
    "Lift" -> pure Lift
    "Sideforce" -> pure Sideforce
    "X Moment" -> pure XMoment
    "Y Moment" -> pure YMoment
    "Z Moment" -> pure ZMoment
    "Efficiency" -> pure Efficiency
    "Equivalent Area" -> pure EquivArea
    "Nearfield Pressure" -> pure NearPressure
    "X Force" -> pure XForce
    "Y Force" -> pure YForce
    "Z Force" -> pure ZForce
    "Thrust" -> pure Thrust
    "Torque" -> pure Torque
    "Total Heat Flux" -> pure TotalHeatFlux
    "Max Heat Flux" -> pure MaxHeatFlux
    "Inverse Design Pressure" -> pure InvDesPressure
    "Inverse Design Heat Flux" -> pure InvDesHeatFlux
    "Surface Total Pressure" -> pure SurfTotPressure
    "Surface Mass Flow" -> pure SurfMassFlow
    "Surface Static Pressure" -> pure SurfStatPressure
    "Surface Mach" -> pure SurfMach
    _ -> fail "Unrecognized SU2 objective"

data MathProb = Direct
              | ContAdjoint
              | DiscAdjoint
instance Render MathProb where
  render Direct = "DIRECT"
  render ContAdjoint = "CONTINUOUS_ADJOINT"
  render DiscAdjoint = "DISCRETE_ADJOINT"

data IncScheme = InitValues
               | RefValues
               | Dim
instance Render IncScheme where
  render InitValues = "INITIAL_VALUES"
  render RefValues  = "REFERENCE_VALUES"
  render Dim        = "DIMENSIONAL"

data GradMethod = GGauss
                | WLS
instance Render GradMethod where
  render GGauss = "GREEN_GAUSS"
  render WLS    = "WEIGHTED_LEAST_SQUARES"

data LinearSolver = FGMRes
                  | RestartFGMRes
                  | BCGStab
                  | SJacobi
                  | SILU
                  | SLUSGS
                  | SLinelet
instance Render LinearSolver where
  render FGMRes        = "FGMRES"
  render RestartFGMRes = "RESTARTED_FGMRES"
  render BCGStab       = "BCGSTAB"
  render SJacobi       = "SMOOTHER_JACOBI"
  render SILU          = "SMOOTHER_ILU"
  render SLUSGS        = "SMOOTHER_LUSGS"
  render SLinelet      = "SMOOTHER_LINELET"

data Preconditioner = ILU
                    | LU_SGS
                    | Linelet
                    | Jacobi
instance Render Preconditioner where
  render ILU     = "ILU"
  render LU_SGS  = "LU_SGS"
  render Linelet = "LINELET"
  render Jacobi  = "JACOBI"

data Stiffness = InvVol
               | WallDist
               | ConstStiff
instance Render Stiffness where
  render InvVol     = "INVERSE_VOLUME"
  render WallDist   = "WALL_DISTANCE"
  render ConstStiff = "CONSTANT_STIFFNESS"

data TimeDiscre = EulerImp
                | RKExp
                | EulerExp
                | RK4Exp
                | AderDG
instance Render TimeDiscre where
  render EulerImp = "EULER_IMPLICIT"
  render RKExp    = "RUNGE-KUTTA_EXPLICIT"
  render EulerExp = "EULER_EXPLICIT"
  render RK4Exp   = "CLASSICAL_RK4_EXPLICIT"
  render AderDG   = "ADER_DG"

data MeshFormat = SU2
instance Render MeshFormat where
  render SU2 = "SU2"

data TabFormat = TECPLOT
               | CSV
instance Render TabFormat where
  render TECPLOT = "TECPLOT"
  render CSV     = "CSV"

data SU2RHS = Solver SU2Solver
            | Boolean Bool
            | ObjectiveFns [Objective]
            | ObjectiveWts [Double]
            | MathProblem MathProb
            | Floating Double
            | Integral Integer
            | MarkerData [(String, Double)]
            | Markers (Maybe [String])
            | IncompressibleScheme IncScheme
            | GradientMethod GradMethod
            | CFLAdaptParam Double Double Double Double Double
            | RKCoefficient Double Double Double
            | LinearSolver LinearSolver
            | Preconditioner Preconditioner
            | TimeDiscre TimeDiscre
            | Filename String
            | MeshFormat MeshFormat
            | TabularFormat TabFormat
            | Stiffness Stiffness
instance Render SU2RHS where
  render (Solver s)                           = render s
  render (Boolean True)                       = "YES"
  render (Boolean False)                      = "NO"
  render (ObjectiveFns fns)                   = intercalate ", " $ render <$> fns
  render (ObjectiveWts wts)                   = intercalate ", " $ show <$> wts
  render (MathProblem mp)                     = render mp
  render (Floating f)                         = show f
  render (Integral i)                         = show i
  render (MarkerData md)                      = "(" ++ intercalate ", " ((\(name, value) -> name ++ ", " ++ show value) <$> md) ++ ")"
  render (Markers Nothing)                    = "NONE"
  render (Markers (Just ms))                  = "(" ++ intercalate ", " ms ++ ")"
  render (IncompressibleScheme is)            = render is
  render (GradientMethod gm)                  = render gm
  render (CFLAdaptParam fd fu minV maxV conv) = "(" ++ intercalate ", " (show <$> [fd, fu, minV, maxV, conv]) ++ ")"
  render (RKCoefficient x y z)                = "(" ++ intercalate ", " (show <$> [x, y, z]) ++ ")"
  render (LinearSolver ls)                    = render ls
  render (Preconditioner p)                   = render p
  render (TimeDiscre t)                       = render t
  render (Filename f)                         = f
  render (MeshFormat mf)                      = render mf
  render (TabularFormat tf)                   = render tf
  render (Stiffness s)                        = render s

instance FromJSON SU2RHS where
  parseJSON (String "Euler")                    = pure (Solver Euler)
  parseJSON (String "Navier Stokes")            = pure (Solver NS)
  parseJSON (String "Wave Equation")            = pure (Solver Wave)
  parseJSON (String "Heat Equation")            = pure (Solver Heat)
  parseJSON (String "Elasticity FEM")           = pure (Solver ElasticityFEM)
  parseJSON (String "Poisson Equation")         = pure (Solver Poisson)
  parseJSON (String "Direct")                   = pure (MathProblem Direct)
  parseJSON (String "Continuous Adjoint")       = pure (MathProblem ContAdjoint)
  parseJSON (String "Discrete Adjoint")         = pure (MathProblem DiscAdjoint)
  parseJSON (String "Initial Values")           = pure (IncompressibleScheme InitValues)
  parseJSON (String "Reference Values")         = pure (IncompressibleScheme RefValues)
  parseJSON (String "Dimensional")              = pure (IncompressibleScheme Dim)
  parseJSON (String "Green-Gauss")              = pure (GradientMethod GGauss)
  parseJSON (String "Weighted Least Squares")   = pure (GradientMethod WLS)
  parseJSON (String "FGMRes")                   = pure (LinearSolver FGMRes)
  parseJSON (String "Restarted FGMRes")         = pure (LinearSolver RestartFGMRes)
  parseJSON (String "BCGStab")                  = pure (LinearSolver BCGStab)
  parseJSON (String "Smoother Jacobi")          = pure (LinearSolver SJacobi)
  parseJSON (String "Smoother ILU")             = pure (LinearSolver SILU)
  parseJSON (String "Smoother LUSGS")           = pure (LinearSolver SLUSGS)
  parseJSON (String "Smoother Linelet")         = pure (LinearSolver SLinelet)
  parseJSON (String "ILU")                      = pure (Preconditioner ILU)
  parseJSON (String "LU SGS")                   = pure (Preconditioner LU_SGS)
  parseJSON (String "Linelet")                  = pure (Preconditioner Linelet)
  parseJSON (String "Jacobi")                   = pure (Preconditioner Jacobi)
  parseJSON (String "Inverse Volume")           = pure (Stiffness InvVol)
  parseJSON (String "Wall Distance")            = pure (Stiffness WallDist)
  parseJSON (String "Constant Stiffness")       = pure (Stiffness ConstStiff)
  parseJSON (String "Euler Implicit")           = pure (TimeDiscre EulerImp)
  parseJSON (String "Runge-Kutta Explicit")     = pure (TimeDiscre RKExp)
  parseJSON (String "Classical RK4 Explicit")   = pure (TimeDiscre RK4Exp)
  parseJSON (String "Ader DG")                  = pure (TimeDiscre AderDG)
  parseJSON (String "SU2")                      = pure (MeshFormat SU2)
  parseJSON (String "TECPlot")                  = pure (TabularFormat TECPLOT)
  parseJSON (String "CSV")                      = pure (TabularFormat CSV)
  parseJSON (String fname)                      = pure $ Filename $ unpack fname
  parseJSON (Number x)                          =
    case floatingOrInteger x of
      Left f  -> pure (Floating f)
      Right i -> pure (Integral i)
  parseJSON (Bool b)                            = pure (Boolean b)

-- | The type of SU2 configuration scripts.
newtype SU2Config = SU2Config { getOptions :: Map String SU2RHS } deriving (FromJSON)
instance Render SU2Config where
  render (SU2Config opts) = Map.foldMapWithKey renderOpt opts
    where
      renderOpt :: String -> SU2RHS -> String
      renderOpt opt val = opt ++ " = " ++ render val ++ "\n"
