{-|
Module      : Language.Error
Description : An error type for LINK
Copyright   : Galois, Inc. 2021
License     : N/A
Maintainer  : cphifer@galois.com
Stability   : Experimental
Portability : N/A

This module defines errors that may be encountered during the compilation of
LINK programs. This includes parsing, linking, type-checking, and translation.

Like many of the other data structures comprising the implementation of LINK,
this set of errors is subject to change as needs/requirements arise.
-}

module Language.Error
  ( LinkError(..)
  ) where

import Data.Class.Render (Render, render)

data LinkError
  = Foo
  | Bar

instance Render LinkError where
  render Foo = "foo"
  render Bar = "bar"
