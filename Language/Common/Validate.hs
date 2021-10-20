{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Language.Common.Validate
Description : Monad for validation against schemas
Copyright   : (c) Galois, Inc. 2021
License     : N/A
Maintainer  : james.lamar@galois.com
Stability   : experimental
Portability : N/A

Definition of and API for the @Val@ monad, which is used for type-checking
against a schema.
-}

module Language.Common.Validate
  ( -- * Schema validation monad
    -- ** Validators and running validators
    Val
  , runVal
    -- ** Accessing types and throwing errors
  , getSchemaDef
  , throw
  ) where

import qualified Control.Monad.Reader   as Reader
import qualified Control.Monad.Validate as Validate

import qualified Data.Map               as Map
import           Data.Text              (Text)

import           Language.Common        (HasLocation, msgWithLoc)
import qualified Language.Schema.Env    as Schema
import qualified Language.Schema.Syntax as Schema

type Ident = Text

type Val a = Validate.ValidateT [Text] (Reader.Reader Schema.Env) a

runVal :: Schema.Env -> Val a -> Either [Text] a
runVal e v = Reader.runReader (Validate.runValidateT v) e

getSchemaDef :: HasLocation why => why -> Ident -> Val Schema.SchemaDef
getSchemaDef why name =
  do  def <- Reader.asks (Map.lookup name . Schema.envTypeDefs)
      case def of
        Nothing ->
          throw why ("[BUG] Could not find definition for schema " <> "'" <> name <> "'" <> " used here")
        Just a -> pure a

throw :: HasLocation a => a -> Text -> Val b
throw why msg = Validate.refute [msgWithLoc why msg]
