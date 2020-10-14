{-|
Module      : Language.Check
Description : Some static checks on LINK programs
Copyright   : (c) Galois, Inc. 2020
License     : N/A
Maintainer  : cphifer@galois.com
Stability   : experimental
Portability : N/A

This module holds some simple static checking that can be done on LINK
programs. They do not constitute full type-checking of any sort; that
work is currently TBD and will likely consist of some cool stuff with
ologs and applied category theory more generally.
-}

module Language.Check
  ( hasAllCouplings
  ) where

import Language.AST

hasAllCouplings :: Prog -> Bool
hasAllCouplings p = undefined
