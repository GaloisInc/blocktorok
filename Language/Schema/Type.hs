{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Language.Schema.Type
Description : Types supported in schemas
Copyright   : (c) Galois, Inc. 2021
License     : N/A
Maintainer  : cphifer@galois.com
Stability   : experimental
Portability : N/A

Types (and associated functions) that are supported in the schema language.
-}

module Language.Schema.Type
  ( -- * Schema Types
    -- ** Type Representations
    Globbed(..)
  , Ident
  , SType(..)
    -- ** User-defined/named type utilities
  , containedName
  , containsNamed
    -- ** Globbed type utilities
  , ppGlob
  , unGlob
  ) where

import           Data.Text                   (Text, unpack)

import           Language.Common.Units.Units (Unit)

-- | Identifiers
type Ident = Text

-- | The types fields may be declared to have in block schemas and constructor
-- variants
data SType
  = SInt Unit
  | SFloat Unit
  | SString
  | SNamed Ident

-- TODO: Add notion of unit equality that makes sense
instance Eq SType where
  SInt _ == SInt _      = True
  SFloat _ == SFloat _  = True
  SIdent == SIdent      = True
  SString == SString    = True
  SList t == SList t'   = t == t'
  SNamed i == SNamed i' = i == i'
  _ == _                = False


instance Show SType where
  show (SInt u)   = "int[" ++ show u ++ "]"
  show (SFloat u) = "float[" ++ show u ++ "]"
  show SString    = "string"
  show (SNamed i) = unpack i

-- | Globs for block layout definitions
data Globbed a
  = One a
  | Optional a
  | Some a
  | Many a
  deriving (Show)

instance Functor Globbed where
  fmap f globbed =
    case globbed of
      One a      -> One $ f a
      Optional a -> Optional $ f a
      Some a     -> Some $ f a
      Many a     -> Many $ f a


-------------------------------------------------------------------------------

-- | Extract a 'Globbed' value
unGlob :: Globbed a -> a
unGlob glob =
  case glob of
    One a      -> a
    Optional a -> a
    Some a     -> a
    Many a     -> a

-- | Pretty-print a 'Globbed' value that can be shown
ppGlob :: Show a => Globbed a -> String
ppGlob g = show (unGlob g) ++ globStr
  where
    globStr = case g of
                One _      -> ""
                Optional _ -> "?"
                Some _     -> "+"
                Many _     -> "*"

-- | Return true iff the type contains a named type
containsNamed :: SType -> Bool
containsNamed (SNamed _) = True
containsNamed _          = False

-- | If the type @t@ contains a named type (i.e. @containsNamed t@ returns
-- @True@), return that type's name. This function errors on types that
-- fail to return true when passed to 'containsNamed'.
containedName :: SType -> Ident
containedName (SNamed i) = i
containedName _          = error "Type contains no named type"
