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

import Data.Void

import Math
import Physics.Model

-- | A complete LINK program, which consists of configuration, a (nonempty)
--   collection of 'Physics.Model.Model's, and a collection of @Coupling@s.
--
--   Given @Prog config models couplings@, we have:
--
--   @length couplings = (length models * (length models - 1)) / 2@
data Prog = Prog Config [Model] [Coupling]

-- | A @Duration@ specifies how long a simulation should run, either as an
--   explicit number of iterations or as an elapsed time.
--   TODO: We need units, and probably shouldn't wait terribly long to do them
data Duration = Iterations Int -- ^ The number of steps to take
              | ElapsedTime Int -- ^ The amount of time to simulate

-- | LINK Configuration, consisting of the global simulation step size and the
--   total @Duration@
data Config =
  Config { getGlobalStep :: Int -- ^ The global solving step size TODO: This should have units
         , getDuration :: Duration -- ^ The duration of the simulation, in time or in #iterations
         }

-- TODO: A coupling relates two models via boundary equations and knowledge of
-- what variables are communicated via the boundary. At minimum, a coupling
-- must consist of two models and the set of consistency/coordination equations
-- that hold at the interface between the models.
type Coupling = Void

-- | A LINK program is a sequence of @Stmt@s
newtype Decl = DStmts [Stmt]

instance Show Decl where
  show (DStmts es) = "\n" ++ concatMap show es ++ " \n\n "

-- | Type representing a top-level statement, which is (currently) an equation
--   or a model specification
data Stmt = Equation Exp Exp Space -- ^ An equation in a particular domain
          | Box Term Model -- ^ A model labeled by a @Term@

instance Show Stmt where
  show (Equation e1 e2 e3) = "\n" ++ show e1 ++ "=" ++ show e2 ++ " in "++ show e3
  show (Box name m)   =  "\n Model " ++ show name ++ ":{" ++ show m ++ "}"
