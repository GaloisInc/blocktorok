{-|
Module      : Language.Schema.Syntax
Description : The abstract syntax of the LINK schema language
Copyright   : (c) Galois, Inc. 2021
License     : N/A
Maintainer  : cphifer@galois.com
Stability   : experimental
Portability : N/A

This module defines the syntax of the LINK schema language, which is used to
define the layout of input files and perform some light validation. Ultimately,
it provides a typing environment to the transformer language, which defines how
input files should be rendered as output to one or more files.
-}

module Language.Schema.Syntax where

import Data.Text (Text)

import Language.Common (Located)

-- | Identifiers
type Ident = Text

-- | The types fields may be declared to have in block schemas and constructor
-- variants
data SType
  = SInt
  | SFloat
  | SIdent
  | SString
  | SList SType
  | SNamed Ident
  deriving (Show)

-- | Union constructor field declarations
data Decl = Decl
  { declName :: Located Ident
  , declType :: Located SType
  } deriving (Show)

-- | Union variant definition
data Variant = Variant
  { variantDoc :: Maybe (Located Text)
  , variantTag :: Located Ident
  , variantFields :: [Decl]
  } deriving (Show)

-- | Union-type definition
data Union = Union
  { unionName :: Located Ident
  , unionVariants :: [Variant]
  } deriving (Show)

-- | Globs for block layout definitions
data Globbed a
  = One a
  | Optional a
  | Some a
  | Many a
  deriving (Show)

-- | Annotated declarations for block layout definitions
data BlockDecl = BlockDecl
  { blockDeclDoc :: Maybe (Located Text)
  , blockDeclDecl :: Decl
  }
  deriving (Show)

-- | Block layout definition
data BlockS = BlockS
  { blockSType :: Located Ident
  , blockSName :: Maybe Decl
  , blockSFields :: [Globbed BlockDecl]
  } deriving (Show)

-- | Root definition; this defines the top-level structure of input files
data Root = Root { rootFields :: [Globbed BlockDecl] } deriving (Show)

-- | Elements comprising schemas (other than the root)
data SchemaDef
  = UnionDef Union
  | BlockDef BlockS
  deriving (Show)

-- | A complete schema is a mix of union definitions and block layouts, and a
-- single root specification
data Schema = Schema
  { schemaDefs :: [SchemaDef]
  , schemaRoot :: Root
  } deriving (Show)
