{-|
Module      : Translation.Su2.Low
Description :  translation from LINK AST to backend
Copyright   : Galois, Inc. 2021
License     : N/A
Maintainer  : chiw@galois.com
Stability   : Experimental
Portability : N/A

This module defines the parser for the LINK language, using Parsec.
-}

module Translation.Su2.Low
  ( Base(..), Stmt(..), Program(..)
  ) where

import Data.Link.AST
import Data.Solver.Backend
import Data.Link.Identifier
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List


data Base = Int Integer | Name String | Neg Base
data Stmt = Constructor Base [Stmt]
  | Equality Base Base
  | Incomplete
data Program = Program [Stmt]
err = Incomplete -- "Should raise an error"
