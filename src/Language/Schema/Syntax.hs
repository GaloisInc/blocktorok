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

import Language.Common (Located, withSameLocAs)

-- | Identifiers
type Ident = Text

-- | The types fields may be declared to have in block schemas
data SType
  = SInt
  | SFloat
  | SString
  | SList SType
  | SUnion Ident
  deriving (Show)

-- | Field declarations
data Decl = Decl
  { declName :: Located Ident
  , declType :: Located SType
  } deriving (Show)

-- | Union variant definition
data Variant = Variant
  { variantDoc :: Maybe Text
  , variantTag :: Located Ident
  , variantFields :: [Decl]
  } deriving (Show)

-- | Union-type definition
data Union = Union
  { unionName :: Located Ident
  , unionVariants :: [Variant]
  } deriving (Show)
