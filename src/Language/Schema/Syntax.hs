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

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

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
  { declName :: Ident
  , declType :: SType
  } deriving (Show)

-- | Union variant definition
data Variant = Variant
  { variantDoc :: Maybe Text
  , variantTag :: Ident
  , variantFields :: Map Ident SType
  } deriving (Show)

-- | Union-type definition
data Union = Union
  { unionName :: Ident
  , unionVariants :: Map Ident Variant
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
  { blockDeclDoc :: Maybe Text
  , blockDeclDecl :: Decl
  }
  deriving (Show)

-- | Block layout definition
data BlockS = BlockS
  { blockSType :: Ident
  , blockSName :: Maybe Decl
  , blockSFields :: Map Ident (Globbed BlockDecl)
  } deriving (Show)

-- | Root definition; this defines the top-level structure of input files
data Root = Root
  { rootFields :: Map Ident (Globbed BlockDecl)
  } deriving (Show)

-- | Elements comprising schemas (other than the root)
data SchemaDef
  = UnionDef Union
  | BlockDef BlockS
  deriving (Show)

-- | A complete schema is a mix of union definitions and block layouts, and a
-- single root specification
data Schema = Schema
  { schemaDefs :: Map Ident SchemaDef
  , schemaRoot :: Root
  } deriving (Show)

-------------------------------------------------------------------------------

unGlob :: Globbed a -> a
unGlob glob =
  case glob of
    One a      -> a
    Optional a -> a
    Some a     -> a
    Many a     -> a

declsMap :: [Decl] -> Map Ident SType
declsMap = Map.fromList . fmap (\d -> (declName d, declType d))

variantsMap :: [Variant] -> Map Ident Variant
variantsMap = Map.fromList . fmap (\v -> (variantTag v, v))

globbedDeclsMap :: [Globbed BlockDecl] -> Map Ident (Globbed BlockDecl)
globbedDeclsMap = Map.fromList . fmap getDecl
  where
    getDecl :: Globbed BlockDecl -> (Ident, Globbed BlockDecl)
    getDecl g = ((declName . blockDeclDecl . unGlob) g, g)

schemaDefMap :: [SchemaDef] -> Map Ident SchemaDef
schemaDefMap = Map.fromList . fmap getDef
  where
    getDef :: SchemaDef -> (Ident, SchemaDef)
    getDef s = (defName s, s)

    defName :: SchemaDef -> Ident
    defName (UnionDef u) = unionName u
    defName (BlockDef b) = blockSType b
