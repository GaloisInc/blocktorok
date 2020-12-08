{-|
Module      : Solver.Technique
Description : Type and associated functions capturing various solving
              techniques
Copyright   : (c) Galois, Inc. 2020
License     : N/A
Maintainer  : cphifer@galois.com
Stability   : experimental
Portability : N/A

This module exports a type distinguishing various types of PDE solving
techniques that can be coupled with other information (type of physics, sets of
equations, etc) to describe a model.

TODO: How tightly coupled should this type be to other things we have? For
example: We might know from 'Physics.Type.PType' what solving technique is most
appropriate, and those should be paired up ahead of time / checked for
consistency at the type level.
-}

module Solver.Technique
  ( Technique(..)
  ) where

import Language.Identifier
-- | The type of a solving technique
data Technique = FEM -- ^ The finite element method
               | FVM -- ^ The finite volume method
               deriving (Show)
