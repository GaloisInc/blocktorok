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
  ( Render
  , render
  ) where

-- | The @Render@ class is for types that should be renderable as 'String's. It
--   is often sufficient for a type's @Render@ instance to simply be equivalent
--   to 'Show', but as a separate class, it allows a distinction between debug
--   and formatted output.
class Render a where
  -- | Render a value as a 'String'.
  render :: a -> String
