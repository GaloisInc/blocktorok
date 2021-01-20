{-|
Module      : Data.Link.Identifier
Description : Representation of LINK identifiers
Copyright   : (c) Galois, Inc. 2020
License     : N/A
Maintainer  : cphifer@galois.com
Stability   : experimental
Portability : N/A

This module defines LINK identifiers (i.e. variable names).
-}

module Data.Link.Identifier
  ( Identifier(..)
  ) where

-- | A simple wrapper around strings for valid LINK identifiers; useful if we
--   want to change this representation to something else later.
newtype Identifier = Identifier String deriving (Eq, Ord, Show)
