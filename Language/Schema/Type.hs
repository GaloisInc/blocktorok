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
    -- ** Globbed type utilities
  , ppGlob
  , unGlob
  ) where

import           Data.Text                   (Text, unpack)
import           Language.Common.Units.Units (Unit (..))

-- | Identifiers
type Ident = Text

-- | The types fields may be declared to have in block schemas and constructor
-- variants
data SType
  = SInt
  | SFloat (Maybe Unit)
  | SString
  | SBool
  | SNamed Ident
  deriving (Eq)

instance Show SType where
  show SInt        = "int"
  show (SFloat mu) =
    "float" ++ case mu of { Nothing -> ""; Just u -> " with dimension of " ++ show u }
  show SString     = "string"
  show SBool       = "boolean"
  show (SNamed i)  = unpack i

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
