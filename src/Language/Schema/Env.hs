{-|
Module      : Language.Schema.Env
Description : Typing evironment generated by schema language
Copyright   : (c) Galois, Inc. 2021
License     : N/A
Maintainer  : cphifer@galois.com
Stability   : experimental
Portability : N/A

The typing environment (and associated functions) computed from the schema
language. This is the interface used by the transformer language to work with
selectors / know their types.
-}

module Language.Schema.Env
  ( Env(..)
  , emptyEnv
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Language.Schema.Syntax
import Language.Schema.Type (Ident, SType)

-- | The typing environment
data Env = Env
  { envRootTypes :: Map Ident SType
  , envTypeDefs :: Map Ident SchemaDef
  }

-- | Construct an empty environment (used as initial parse state)
emptyEnv :: Env
emptyEnv = Env Map.empty Map.empty

addRootType :: Ident -> SType -> Map Ident SType -> Map Ident SType
addRootType = Map.insert

addTypeDef :: Ident -> SchemaDef -> Map Ident SchemaDef -> Map Ident SchemaDef
addTypeDef = Map.insert
