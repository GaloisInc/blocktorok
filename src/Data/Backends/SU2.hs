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

data SU2Solver = Euler
               | NS
               | Rans
               | EulerInc
               | NSInc
               | RansInc
               | EulerNemo
               | NSNemo
               | EulerFEM
               | NSFEM
               | RansFEM
               | LesFEM
               | HeatFVM
               | Elasticity

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

data SU2Config =
  SU2Config { getSolver :: SU2Solver
            , getObjectiveFn :: Objective -- TODO: Support multiple
            , getObjectiveWt :: Double -- TODO: Support multiple
            , isTimeDomain :: Bool
            , getTimeStep :: Double -- TODO: Units?
            , getMaxTime :: Double -- TODO: Units?
            , getInternalIter :: Integer
            , getTimeIter :: Integer
            }
