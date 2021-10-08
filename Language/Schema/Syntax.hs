{-|
Module      : Language.Schema.Syntax
Description : The abstract syntax of the Blocktorok schema language
Copyright   : (c) Galois, Inc. 2021
License     : N/A
Maintainer  : cphifer@galois.com
Stability   : experimental
Portability : N/A

This module defines the syntax of the Blocktorok schema language, which is used
to define the layout of input files and perform some light validation.
Ultimately, it provides a typing environment to the transformer language, which
defines how input files should be rendered as output to one or more files.
-}

module Language.Schema.Syntax
  ( -- * Blocktorok Schemas
    -- ** The AST
    BlockDecl(..)
  , BlockS(..)
  , Decl(..)
  , Root(..)
  , Schema(..)
  , SchemaDef(..)
  , Union(..)
  , Variant(..)
    -- ** Turning lists into 'Map's
  , declsMap
  , globbedDeclsMap
  , schemaDefMap
  , variantsMap
  ) where

import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           Data.Text            (Text, unpack)

import           Language.Common      (Located (..))
import           Language.Schema.Type (Globbed, Ident, SType, unGlob)

-- | Union constructor field declarations
data Decl = Decl
  { declName :: Located Ident
  , declType :: Located SType
  }

instance Show Decl where
  show (Decl n t) = unpack (locValue n) ++ ":" ++ show (locValue t)

-- | Union variant definition
data Variant = Variant
  { variantDoc :: Maybe (Located Text)
  , variantTag :: Located Ident
  , variantArg :: Maybe SType
  } deriving (Show)

-- | Union-type definition
data Union = Union
  { unionName     :: Located Ident
  , unionVariants :: Map Ident Variant
  } deriving (Show)

-- | Annotated declarations for block layout definitions
data BlockDecl = BlockDecl
  { blockDeclDoc  :: Maybe (Located Text)
  , blockDeclDecl :: Decl
  }

instance Show BlockDecl where
  show (BlockDecl _ d) = show d

-- | Block layout definition
data BlockS = BlockS
  { blockSType   :: Located Ident
  , blockSFields :: Map Ident (Globbed BlockDecl)
  } deriving (Show)

-- | Root definition; this defines the top-level structure of input files
newtype Root = Root
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

-- | Transform a list of 'Decl's into a map from 'Ident' to 'SType'
declsMap :: [Decl] -> Map Ident SType
declsMap =
  Map.fromList . fmap (\d-> (locValue (declName d), locValue (declType d)))

-- | Transform a list of 'Variant's into a map from 'Ident' to 'Variant'
-- where the 'Ident' is the variant's tag
variantsMap :: [Variant] -> Map Ident Variant
variantsMap = Map.fromList . fmap (\v -> (locValue (variantTag v), v))

-- | Transform a list of 'Globbed' 'BlockDecl's into a map
globbedDeclsMap :: [Globbed BlockDecl] -> Map Ident (Globbed BlockDecl)
globbedDeclsMap = Map.fromList . fmap getDecl
  where
    getDecl :: Globbed BlockDecl -> (Ident, Globbed BlockDecl)
    getDecl g = ((locValue . declName . blockDeclDecl . unGlob) g, g)

-- | Transform a list of 'SchemaDef's into a map
schemaDefMap :: [SchemaDef] -> Map Ident SchemaDef
schemaDefMap = Map.fromList . fmap getDef
  where
    getDef :: SchemaDef -> (Ident, SchemaDef)
    getDef s = (defName s, s)

    defName :: SchemaDef -> Ident
    defName (UnionDef u) = locValue (unionName u)
    defName (BlockDef b) = locValue (blockSType b)
