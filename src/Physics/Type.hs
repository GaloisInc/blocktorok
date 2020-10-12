{-|
Module      : Physics.Type
Description : Simple type / associated functions for distinguishing types of
              physics
Copyright   : (c) Galois, Inc. 2020
License     : N/A
Maintainer  : cphifer@galois.com
Stability   : experimental
Portability : N/A

This module exports a type allowing for the distinguishing of different types
of physics at the type level. Its main use is tagging records of type
'Physics.Model.Model'.
-}

module Physics.Type
  ( PType(..)
  ) where

-- | Tag type for physical models indicating what they are modeling
data PType = LaminarFlow  -- ^ Laminar fluid flow
           | HeatTransfer -- ^ Heat transfer between entities
           deriving (Show)
--- Note this is hard-coded into the Parser and not yet added to the grammar 
