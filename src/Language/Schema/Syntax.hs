{-# LANGUAGE OverloadedStrings #-}

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

-- | The types fields may be declared to have in block schemas
data SType
  = SInt
  | SFloat
  | SString
  | SList SType
  | SNamed Ident
  deriving (Show)

-- | Union constructor field declarations
data UDecl = UDecl
  { udeclName :: Located Ident
  , udeclType :: Located SType
  } deriving (Show)

-- | Union variant definition
data Variant = Variant
  { variantDoc :: Maybe (Located Text)
  , variantTag :: Located Ident
  , variantFields :: [UDecl]
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

-- | Block field declarations
data BDecl = BDecl
  { bdeclDoc :: Maybe (Located Text)
  , bdeclName :: Located Ident
  , bdeclType :: Located SType
  }
  deriving (Show)

-- | Block layout definition
data BlockS = BlockS
  { blockType :: Located Ident
  , blockFields :: [Globbed BDecl]
  } deriving (Show)
