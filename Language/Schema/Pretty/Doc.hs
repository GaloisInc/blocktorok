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

import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map

import           Data.Text                 (Text)

import           Prettyprinter             (Doc, Pretty (pretty), align,
                                            concatWith, emptyDoc, enclose,
                                            hardline, (<+>))
import           Prettyprinter.Render.Text (putDoc)

import qualified System.FilePath           as Path

import           Language.Common           (orThrow, unloc)
import           Language.Schema.Parser    (schemaASTFromFile)
import           Language.Schema.Pretty    (blankLine, ppNoLoc, ppStype, (<//>), (</>))
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
           <//> ppRoot schemaRoot
           <//> ppSchemaDefs schemaDefs
             <> hardline
     putDoc docs

-------------------------------------------------------------------------------
-- Generating 'Doc's from 'Schema's

ppRoot :: Root -> Doc ann
ppRoot Root { rootFields } =
       h2 "Top-level structure"
  <//> "The following must appear at the top-level of your data file:"
  <//> ppFields rootFields

ppFields :: Map Ident (Globbed BlockDecl) -> Doc ann
ppFields gbds = concatWith (</>) (Map.elems (Map.map ppField gbds))
  where
    ppField :: Globbed BlockDecl -> Doc ann
    ppField gbd = item $
      case gbd of
        One bd      -> ppDeclWith "Exactly one field" bd
        Optional bd -> ppDeclWith "At most one field" bd
        Some bd     -> ppDeclWith "At least one field" bd
        Many bd     -> ppDeclWith "Any number of fields" bd

    ppDeclWith :: Text -> BlockDecl -> Doc ann
    ppDeclWith msg BlockDecl { blockDeclDoc, blockDeclDecl } = align $
          pretty msg
      <+> ppDecl blockDeclDecl
       <> maybe emptyDoc (\bdd -> hardline <> ppNoLoc bdd) blockDeclDoc

    ppDecl :: Decl -> Doc ann
    ppDecl Decl { declName , declType } =
          italic (pretty (unloc declName))
      <+> "with type"
      <+> bold (ppStype (unloc declType))
       <> "."

ppSchemaDefs :: Map Ident SchemaDef -> Doc ann
ppSchemaDefs defs =
       h2 "Block and union types"
  <//> concatWith (<//>) (Map.map ppSchemaDef defs)
  where
    ppSchemaDef :: SchemaDef -> Doc ann
    ppSchemaDef (UnionDef Union { unionName, unionVariants }) =
           h3 (bold (pretty (unloc unionName)))
      <//> "Which has constructors:"
      <//> ppVariants unionVariants
    ppSchemaDef (BlockDef BlockS { blockSType, blockSFields }) =
           h3 (bold (pretty (unloc blockSType)))
      <//> "Blocks of this type must contain:"
      <//> ppFields blockSFields

    ppVariants :: Map Ident Variant -> Doc ann
    ppVariants variants =
      concatWith (<//>) (Map.elems (Map.map ppVariant variants))

    ppVariant :: Variant -> Doc ann
    ppVariant Variant { variantDoc, variantTag, variantFields} =
           h4 (italic (pretty (unloc variantTag)))
        <> maybe emptyDoc (\vd -> blankLine <> ppNoLoc vd) variantDoc
        <> if Map.null variantFields
           then emptyDoc
           else blankLine
             <> "This variant carries data:"
           <//> ppVariantFields variantFields

    ppVariantFields :: Map Ident SType -> Doc ann
    ppVariantFields types =
      concatWith (</>) (Map.elems (Map.mapWithKey ppTypeDecl types))

    ppTypeDecl :: Ident -> SType -> Doc ann
    ppTypeDecl nm t =
          item (italic (pretty nm))
      <+> "with type"
      <+> bold (ppStype t)

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
