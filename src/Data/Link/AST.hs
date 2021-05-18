{-|
Module      : Data.Link.AST
Description : The LINK AST
Copyright   : (c) Galois, Inc. 2020
License     : N/A
Maintainer  : chiw@galois.com
Stability   : experimental
Portability : N/A

This module exports the abstract representation of the LINK concrete syntax. It
is the target of the parser defined in @Parser.y@.
-}

module Data.Link.AST
  ( Config(..)
  , Coupling(..)
  , Duration(..)
  , Prog(..)
  , RunFn(..)
  , MeshFileTy(..), TimeDomainTy(..), CoupledSurfacesTy(..)
  ) where

import Data.Map.Strict (Map)
import Data.Link.Identifier (Identifier)
import Data.Physics.Model (Model)
import Data.Units.UnitExp (UnitExp)
import Data.Solver.Backend (BackendConfig)
import qualified Data.Equation as Eqn

import Language.Haskell.TH.Syntax (Name)

-- | A complete LINK program, which consists of configuration, a (nonempty)
--   collection of 'Physics.Model.Model's, and a collection of @Coupling@s.
--
--   Given a term @Prog config models couplings@, we have:
--
--   @length couplings = (length models * (length models - 1)) / 2@
data Prog =
  Prog { getConfig :: Config -- ^ The global configuration
       --, getSolvingTechnique :: SolvingTechnique
       --, getNumericalScheme :: NumericalScheme
       , getModels :: Map Identifier Model -- ^ The specified models
       , getCouplings :: [Coupling] -- ^ The model couplings
       }
instance Show Prog where
     show (Prog c1 m c2 ) = "\n\n\t" ++ show c1
      ++ "\n\n\t" ++ show m
      ++ "\n\n\t" ++ show c2
      -- ++ "\n\n\t" ++ show s
      -- ++ "\n\n\t" ++ show n

data TimeDomainTy = Transient | Steady
              deriving (Show)

-- | A @Duration@ specifies how long a simulation should run, either as an
--   explicit number of iterations or as an elapsed time.
--   TODO: We need units, and probably shouldn't wait terribly long to do them
data Duration = IterationsTime Double (UnitExp Name Name) -- ^ The number of steps to take
              | TotalTime Double  (UnitExp Name Name) -- ^ The amount of time to simulate
              deriving (Eq, Show)

data RunFn = RFn Identifier Identifier
              deriving (Show)
data MeshFileTy = MeshFile Identifier Identifier
              deriving (Show)

-- | LINK Configuration, consisting of the global simulation step size and the
--   total @Duration@
data Config =
  Config { getTimeDomain :: TimeDomainTy
         , getGlobalStep :: (Double, UnitExp Name Name) -- ^ The global solving step size TODO: This should have units
         , getDuration :: Duration  -- ^ The duration of the simulation, in time or in #iterations
         , getCouplingIterations :: Integer
         , getConsts :: Map Identifier (Double, UnitExp Name Name)
         , getRunFn :: RunFn
         , getMesh :: MeshFileTy
         , getBackendConfig ::BackendConfig
         } deriving (Show)

data CoupledSurfacesTy =  CoupledSurfaces Identifier Identifier Identifier
  deriving (Show)

-- TODO: A coupling relates two models via boundary equations and knowledge of
-- what variables are communicated via the boundary. At minimum, a coupling
-- must consist of two models and the set of consistency/coordination equations
-- that hold at the interface between the models.
data Coupling =
  Coupling { name :: Identifier
           , model1 :: Identifier
           , model2 :: Identifier
           , input :: Identifier
           , output :: [Identifier]
           , coupledSurfaces :: CoupledSurfacesTy
           , getVars :: Map Identifier (UnitExp Name Name)
           , getEqs :: [Eqn.Equation] -- ^ The equations governing the model
           } deriving (Show)
