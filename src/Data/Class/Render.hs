{-|
Module      : Data.Class.Render
Description : Type class for rendering datatypes
Copyright   : (c) Galois, Inc. 2021
License     : N/A
Maintainer  : cphifer@galois.com
Stability   : experimental
Portability : N/A

This module defines a typeclass for data renderable as a @String@. It is often
equivalent to @Show@, but may be distinct as it is intended for writing output.
-}

module Data.Class.Render
  where

class Render a where
  render :: a -> String
