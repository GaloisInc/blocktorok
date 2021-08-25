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
  , addRootType
  , addTypeDef
  , emptyEnv
  , lookupTypeDef
  ) where

import           Control.Monad.State    (MonadState (get, put))

import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import           Data.Text              (unpack)

import           Language.Common        (locValue)
import           Language.Schema.Syntax (BlockDecl (blockDeclDecl),
                                         Decl (declName), SchemaDef)
import           Language.Schema.Type   (Globbed, Ident, ppGlob, unGlob)
import qualified Debug.Trace as Trace

-- | The typing environment
data Env = Env
  { envRootTypes :: Map Ident (Globbed BlockDecl)
  , envTypeDefs  :: Map Ident SchemaDef
  }

-- | Construct an empty environment (used as initial parse state)
emptyEnv :: Env
emptyEnv = Env Map.empty Map.empty

-- | Add a new root type to the 'Env' if it is not already defined, failing
-- with an error otherwise
addRootType :: (MonadState Env m, MonadFail m)
            => Globbed BlockDecl
            -> m ()
addRootType d =
  do e <- get
     case Map.lookup i (envRootTypes e) of
       Just d' ->
         fail $ "Cannot add root type " ++ ppGlob d ++
                " since the definition " ++ ppGlob d' ++
                " already exists in the root specification."
       Nothing ->
         do  put $ e { envRootTypes = Map.insert i d $ envRootTypes e }
             Trace.traceM "Added:"
             Trace.traceShowM (i, d)

  where i =  (locValue . declName . blockDeclDecl . unGlob) d

-- | Add a new type definition to the `Env` if it is not already defined,
-- failing with an error otherwise
addTypeDef :: (MonadState Env m, MonadFail m)
           => Ident
           -> SchemaDef
           -> m ()
addTypeDef i s =
  do e <- get
     case Map.lookup i (envTypeDefs e) of
       Just _ ->
         fail $ "The type " ++ show (unpack i) ++ " is already defined."
       Nothing -> put $ e { envTypeDefs = Map.insert i s $ envTypeDefs e }

-- | Lookup an identifier in the 'Env's type definition map, returning
-- 'Nothing' if it is not found
lookupTypeDef :: Ident -> Env -> Maybe SchemaDef
lookupTypeDef i e = Map.lookup i (envTypeDefs e)
