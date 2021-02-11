{-|
Module      : Translation.Su2.Low
Description :  Low IR that represents SU2 code
Copyright   : Galois, Inc. 2021
License     : N/A
Maintainer  : chiw@galois.com
Stability   : Experimental
Portability : N/A

-}

module Translation.Su2.Low
  ( Base(..), Stmt(..), LowIR (..)
  ) where

import Data.Link.AST
import Data.Solver.Backend
import Data.Link.Identifier
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List

-- Base expressions
data Base = Int Integer | Name String | Neg Base | Pair [Base]

-- Statements
data Stmt = Constructor Base [Stmt]
  | Equality Base Base

-- Lower IR
data LowIR = Program [Stmt]
