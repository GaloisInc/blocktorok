{-|
Module      : Language.Compile.SU2
Description : Translation from the LINK AST to the SU2 lower IR
Copyright   : (c) Galois, Inc. 2021
License     : N/A
Maintainer  : cphifer@galois.com
Stability   : experimental
Portability : N/A

This module provides a translation between the LINK abstract syntax and SU2
configurations.
-}

module Language.Compile.SU2
  (
  ) where

import Data.Link.AST
import Data.Backends.SU2

compile :: Prog -> SU2Config
compile = undefined
