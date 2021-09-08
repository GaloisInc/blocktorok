{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Language.Schema.Pretty.Template
Description : Blocktorok data template generation from schemas
Copyright   : (c) Galois, Inc. 2021
License     : N/A
Maintainer  : cphifer@galois.com
Stability   : experimental
Portability : N/A

Generation of pretty-printed Blocktorok data templates from a Blocktorok
schema. This is an experimental feature that will certainly require tuning to
make sure the generated template is correct.
-}

module Language.Schema.Pretty.Template
  ( ppSchemaTemplate
  ) where

import           Prettyprinter             (Doc, Pretty (pretty), hardline,
                                            (<+>))
import           Prettyprinter.Render.Text (putDoc)

import qualified System.FilePath           as Path

import           Language.Common           (orThrow, unloc)
import           Language.Schema.Parser    (schemaASTFromFile)
import           Language.Schema.Pretty
import           Language.Schema.Syntax    (Schema (..))

import           Link                      (LinkError (..))

-- | Generate and print a data template from a schema file
ppSchemaTemplate :: FilePath -> IO ()
ppSchemaTemplate fp =
  do Schema { schemaDefs, schemaRoot } <- schemaASTFromFile fp `orThrow` ParseError
     let docs = comment "Template generated from" <+> pretty (Path.dropExtensions fp)
             <> hardline
     putDoc docs

-------------------------------------------------------------------------------
-- Conveniences

comment :: Doc ann -> Doc ann
comment x = "--" <+> x
