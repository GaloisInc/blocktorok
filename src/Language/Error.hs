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
import Data.Link.AST (Duration)
import Data.Link.Identifier (Identifier(..))
import Data.Physics.Model (PhysicsType)
import Data.Units.UnitExp (UnitExp)

import Language.Haskell.TH.Syntax (Name)

import Text.Parsec (ParseError)

data LinkError
  = ParseError ParseError
  | NoPrograms
  | NoModelWithName String
  | MismatchedGSs (Integer, UnitExp Name Name) (Integer, UnitExp Name Name)
  | MismatchedDur Duration Duration
  | MismatchedConstVal Identifier Integer Integer
  | MismatchedConstUnit Identifier (UnitExp Name Name) (UnitExp Name Name)
  | UnknownFormat String
  | UnknownPhysParams String
  | UnknownSolvingTech String
  | UnknownNumScheme String
  | UnsupportedPhys PhysicsType
  | NYI -- Placeholder for anything we have TODO

instance Render LinkError where
  render (ParseError pe) = "A parsing error occurred: " ++ show pe
  render NoPrograms = "No programs provided. This is probably an internal error!"
  render (NoModelWithName m) = "Could not find model with name \"" ++ m ++ "\""
  render (MismatchedGSs gs gs') = "Expected a global step of " ++ show gs ++ " but found a conflict: " ++ show gs'
  render (MismatchedDur dur dur') = "Expected a duration of " ++ show dur ++ " but found a conflict: " ++ show dur'
  render (MismatchedConstVal (Identifier ident) v v') =
    "Expected constant " ++ ident ++ " to have value " ++ show v ++ " but it has value " ++ show v' ++ " in some source"
  render (MismatchedConstUnit (Identifier ident) u u') =
    "Expected constant " ++ ident ++ " to have units of " ++ show u ++ " but it has units of " ++ show u' ++ " in some source"
  render (UnknownFormat fmt) = "Unknown format configuration: " ++ fmt
  render (UnknownPhysParams pParams) = "Unknown physics parameters: " ++ pParams
  render (UnknownSolvingTech st) = "Unknown solving technique: " ++ st
  render (UnknownNumScheme ns) = "Unknown numerical scheme: " ++ ns
  render (UnsupportedPhys pType) = "Unsupported physics type: " ++ show pType
  render NYI = "Not yet implemented"
