{-|
Module      : Language.AST
Description : The LINK AST
Copyright   : (c) Galois, Inc. 2020
License     : N/A
Maintainer  : chiw@galois.com
Stability   : experimental
Portability : N/A

This module exports the abstract representation of the LINK concrete syntax. It
is the target of the parser defined in @Parser.y@.
-}

module Language.AST where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Physics.Model

-- | A simple wrapper around strings for valid LINK identifiers; useful if we
--   want to change this representation to something else later.
newtype Identifier = Identifier String deriving (Eq, Ord, Show)

-- | A complete LINK program, which consists of configuration, a (nonempty)
--   collection of 'Physics.Model.Model's, and a collection of @Coupling@s.
--
--   Given a term @Prog config models couplings@, we have:
--
--   @length couplings = (length models * (length models - 1)) / 2@
data Prog =
  Prog { getConfig :: Config -- ^ The global configuration
       , getModels :: Map Identifier Model -- ^ The specified models
       , getCouplings :: [Coupling] -- ^ The model couplings
       } deriving (Show)

-- | A @Duration@ specifies how long a simulation should run, either as an
--   explicit number of iterations or as an elapsed time.
--   TODO: We need units, and probably shouldn't wait terribly long to do them
data Duration = Iterations Int -- ^ The number of steps to take
              | TotalTime Int -- ^ The amount of time to simulate
              deriving (Show)

-- | LINK Configuration, consisting of the global simulation step size and the
--   total @Duration@
data Config =
  Config { getGlobalStep :: Int -- ^ The global solving step size TODO: This should have units
         , getDuration :: Duration -- ^ The duration of the simulation, in time or in #iterations
         } deriving (Show)

-- TODO: A coupling relates two models via boundary equations and knowledge of
-- what variables are communicated via the boundary. At minimum, a coupling
-- must consist of two models and the set of consistency/coordination equations
-- that hold at the interface between the models.
data Coupling = Coupling Identifier Identifier deriving (Show)
