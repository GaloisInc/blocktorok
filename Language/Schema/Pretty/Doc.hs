{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Language.Schema.Pretty.Doc
Description : Markdown documentation generation from schemas
Copyright   : (c) Galois, Inc. 2021
License     : N/A
Maintainer  : cphifer@galois.com
Stability   : experimental
Portability : N/A

Generation of pretty-printed Markdown documentation from a Blocktorok schema.
This is an experimental feature that will certainly require tuning to make sure
the generated documentation is clear and well laid-out.
-}

module Language.Schema.Pretty.Doc
  ( ppSchemaDocs
  ) where

import qualified Control.Exception         as Ex

import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map

import           Data.Text                 (Text)

import           Prettyprinter             (Doc, Pretty (pretty), align,
                                            concatWith, emptyDoc, enclose,
                                            hardline, (<+>))
import           Prettyprinter.Render.Text (putDoc)

import qualified System.FilePath           as Path

import           Language.Common           (unloc)
import           Language.Schema.Parser    (schemaASTFromFile)
import           Language.Schema.Pretty    (blankLine, noLocDoc, (<//>), (</>))
import           Language.Schema.Syntax    (BlockDecl (..), BlockS (..),
                                            Decl (..), Root (..), Schema (..),
                                            SchemaDef (..), Union (..),
                                            Variant (..))
import           Language.Schema.Type      (Globbed (..), Ident, SType (..))

import           Link                      (LinkError (..))

-- | Generate and print documentation from a schema file
ppSchemaDocs :: FilePath -> IO ()
ppSchemaDocs fp =
  do Schema { schemaDefs, schemaRoot } <- schemaASTFromFile fp `orThrow` ParseError
     let docs = h1 "Documentation for" <+> pretty (Path.dropExtensions fp)
           <//> rootDoc schemaRoot
           <//> schemaDefsDoc schemaDefs
             <> hardline
     putDoc docs

-------------------------------------------------------------------------------
-- Generating 'Doc's from 'Schema's

rootDoc :: Root -> Doc ann
rootDoc Root { rootFields = rfs } =
       h2 "Top-level structure"
  <//> "The following must appear at the top-level of your data file:"
  <//> fieldsDoc rfs

stypeDoc :: SType -> Doc ann
stypeDoc t =
  case t of
    SInt       -> "int"
    SFloat     -> "float"
    SString    -> "string"
    SNamed txt -> pretty txt

fieldsDoc :: Map Ident (Globbed BlockDecl) -> Doc ann
fieldsDoc gbds = concatWith (</>) (Map.elems (Map.map fieldDoc gbds))
  where
    fieldDoc :: Globbed BlockDecl -> Doc ann
    fieldDoc gbd = item $
      case gbd of
        One bd      -> globbedFieldDoc "Exactly one field" bd
        Optional bd -> globbedFieldDoc "At most one field" bd
        Some bd     -> globbedFieldDoc "At least one field" bd
        Many bd     -> globbedFieldDoc "Any number of fields" bd

    globbedFieldDoc :: Text -> BlockDecl -> Doc ann
    globbedFieldDoc msg BlockDecl { blockDeclDoc, blockDeclDecl } = align $
          pretty msg
      <+> declDoc blockDeclDecl
       <> maybe emptyDoc (\bdd -> hardline <> noLocDoc bdd) blockDeclDoc

    declDoc :: Decl -> Doc ann
    declDoc Decl { declName , declType } =
          italic (pretty (unloc declName))
      <+> "with type"
      <+> bold (stypeDoc (unloc declType))
       <> "."

schemaDefsDoc :: Map Ident SchemaDef -> Doc ann
schemaDefsDoc defs =
       h2 "Block and union types"
  <//> concatWith (<//>) (Map.map schemaDefDoc defs)
  where
    schemaDefDoc :: SchemaDef -> Doc ann
    schemaDefDoc (UnionDef Union { unionName, unionVariants }) =
           h3 (bold (pretty (unloc unionName)))
      <//> "Which has constructors:"
      <//> variantsDoc unionVariants
    schemaDefDoc (BlockDef BlockS { blockSType, blockSFields }) =
           h3 (bold (pretty (unloc blockSType)))
      <//> "Blocks of this type must contain:"
      <//> fieldsDoc blockSFields

    variantsDoc :: Map Ident Variant -> Doc ann
    variantsDoc variants =
      concatWith (<//>) (Map.elems (Map.map variantDoc' variants))

    variantDoc' :: Variant -> Doc ann
    variantDoc' Variant { variantDoc, variantTag, variantFields} =
           h4 (italic (pretty (unloc variantTag)))
        <> maybe emptyDoc (\vd -> blankLine <> noLocDoc vd) variantDoc
        <> if Map.null variantFields
           then emptyDoc
           else blankLine
             <> "This variant carries data:"
           <//> variantFieldsDoc variantFields

    variantFieldsDoc :: Map Ident SType -> Doc ann
    variantFieldsDoc types =
      concatWith (</>) (Map.elems (Map.mapWithKey tyDeclDoc types))

    tyDeclDoc :: Ident -> SType -> Doc ann
    tyDeclDoc nm t =
          item (italic (pretty nm))
      <+> "with type"
      <+> bold (stypeDoc t)

-------------------------------------------------------------------------------
-- Simple Markdown annotations

h1 :: Doc ann -> Doc ann
h1 = ("#" <+>)

h2 :: Doc ann -> Doc ann
h2 = ("##" <+>)

h3 :: Doc ann -> Doc ann
h3 = ("###" <+>)

h4 :: Doc ann -> Doc ann
h4 = ("####" <+>)

italic :: Doc ann -> Doc ann
italic = enclose "*" "*"

bold :: Doc ann -> Doc ann
bold = enclose "**" "**"

item :: Doc ann -> Doc ann
item = ("-" <+>)

-------------------------------------------------------------------------------
-- Internals

orThrow :: Ex.Exception c => IO (Either a b) -> (a -> c) -> IO b
orThrow io mkC  =
  do  eitherB <- io
      case eitherB of
        Left a  -> Ex.throwIO (mkC a)
        Right b -> pure b
