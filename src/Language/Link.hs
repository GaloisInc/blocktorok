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
  (
  ) where

import Data.Link.AST (Prog)

-- | Given a list of LINK programs, synthesize a single LINK program built by
-- taking a 'union' over everything in the list. This union is intelligent in
-- that it checks for certain kinds of compatibility problems, such as
-- model variables declaed with different units. The behavior of this function
-- will likely change as LINK evolves.
link :: [Prog] -> Either String Prog
link ps = Left "Not yet implemented"
