{-|
Module      : Language.Link
Description : Linking for the LINK language
Copyright   : Galois, Inc. 2021
License     : N/A
Maintainer  : cphifer@galois.com
Stability   : Experimental
Portability : N/A

This module defines functions for linking multiple LINK source files into a
single source for compilation. It provides some base-level static analyses
around model compatibility.

At its core, though, this linker is simply performing a union-like operation
on the source files - For two models in the sources with identical names, their
components will be collected, checked for consistency, and used to construct
a single model with all of the equations, variables, etc.
-}

module Language.Link
  ( link
  ) where

import Control.Monad.Except (Except, throwError)

import Data.Link.AST (Config(..), Duration, Prog(..))
import Data.Physics.Model (Model)
import Data.Units.UnitExp (UnitExp)
import Language.Error (LinkError(..))

import Data.List (find)

import Language.Haskell.TH.Syntax (Name)

-- | Given a list of LINK programs, synthesize a single LINK program built by
-- taking a 'union' over everything in the list. This union is intelligent in
-- that it checks for certain kinds of compatibility problems, such as
-- model variables declaed with different units. The behavior of this function
-- will likely change as LINK evolves.
link :: [Prog] -> Except LinkError Prog
link []     = throwError NoPrograms
link (p:ps) =
  do -- Destructure the first program to support the various checks
     let Prog { getConfig = Config { getGlobalStep = gs
                                   , getDuration = dur
                                   , getConsts = consts
                                   , getRunFn = rfn
                                   , getBackendConfig = bcConfig }
              , getModels = models
              , getCouplings = couplings } = p

     -- Check that all global steps are the same, throwing an error otherwise
     checkGSteps gs
     checkDurs dur
     return p
  where
    gSteps :: [(Integer, UnitExp Name Name)]
    gSteps = (\Prog { getConfig = Config { getGlobalStep = gs } } -> gs) <$> ps

    durs :: [Duration]
    durs = (\Prog { getConfig = Config { getDuration = dur } } -> dur) <$> ps

    checkGSteps :: (Integer, UnitExp Name Name) -> Except LinkError ()
    checkGSteps gs =
      do case find (/= gs) gSteps of
           Just gs' -> throwError $ MismatchedGSs gs gs'
           Nothing -> return ()

    checkDurs :: Duration -> Except LinkError ()
    checkDurs dur =
      do case find (/= dur) durs of
           Just dur' -> throwError $ MismatchedDur dur dur'
           Nothing -> return ()

    linkModels :: [Model] -> Except LinkError Model
    linkModels _ = throwError NYI
