{-# LANGUAGE FlexibleContexts #-}

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

import Control.Monad.State

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Language.Schema.Syntax
import Language.Schema.Type (Ident, Globbed)

-- | The typing environment
data Env = Env
  { envRootTypes :: Map Ident (Globbed BlockDecl)
  , envTypeDefs :: Map Ident SchemaDef
  }

-- | Construct an empty environment (used as initial parse state)
emptyEnv :: Env
emptyEnv = Env Map.empty Map.empty

addRootType :: (MonadState Env m, MonadFail m)
            => Ident
            -> Globbed BlockDecl
            -> m ()
addRootType i d =
  do e <- get
     case Map.lookup i (envRootTypes e) of
       Just d' -> fail "Placeholder"
       Nothing -> put $ e { envRootTypes = Map.insert i d $ envRootTypes e }

addTypeDef :: (MonadState Env m, MonadFail m)
           => Ident
           -> SchemaDef
           -> m ()
addTypeDef i s =
  do e <- get
     case Map.lookup i (envTypeDefs e) of
       Just s' -> fail "Placeholder"
       Nothing -> put $ e { envTypeDefs = Map.insert i s $ envTypeDefs e }
