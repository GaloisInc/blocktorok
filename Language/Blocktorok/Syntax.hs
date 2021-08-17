{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Language.Blocktorok.Syntax
Description : Blocktorok data language syntax definition
Copyright   : (c) Galois, Inc. 2021
License     : N/A
Maintainer  : james.lamar@galois.com
Stability   : experimental
Portability : N/A

Definition of the AST for Blocktorok data; for definitions of schema language
syntax see "Language.Schema.Syntax", and for the transformer language see
"Language.Transformer.Syntax".
-}

module Language.Blocktorok.Syntax
  ( -- * Blocktorok Data
    -- ** The AST
    Block(..)
  , BlockElement(..)
  , Constructor(..)
  , Ident
  , Value(..)
    -- ** Utility functions
  , locateValue
  ) where

import           Data.Text       (Text)

import           Language.Common (Located, withSameLocAs)

-- | Identifiers; type alias for easy representation changes in the future
type Ident = Text

-- | Top-level structure of Blocktorok data blocks
data Block = Block
  { -- | The "type name" of the block
    blockTypeName :: Located Ident
    -- | An optional identifier for the block, intended to distinguish blocks
    -- with the same 'blockTypeName'
  , blockName     :: Maybe (Located Ident)
    -- | The fields and sub-blocks contained in the block
  , blockContents :: [BlockElement]
  }
  deriving(Show, Eq, Ord)

-- | Data that may appear within a Blocktorok 'Block'
data BlockElement =
    BlockSub (Located Block)
  | BlockValue (Located Ident) Value
  deriving(Show, Eq, Ord)

-- | Blocktorok values, corresponding to the types fields may be declared to
-- have in a valid schema
data Value =
    Number (Located Double)
  | Ident (Located Ident)
  | List (Located [Value])
  | Construct (Located Constructor)
  | String (Located Text)
  deriving(Show, Eq, Ord)

-- | Values inhabiting union types
data Constructor = Constructor
  { constructorName   :: Located Ident
  , constructorFields :: [(Located Ident, Value)]
  }
  deriving(Show, Eq, Ord)

-- | Locate a 'Value' where its underlying data is located
locateValue :: Value -> Located Value
locateValue v =
  case v of
    Number n    -> v `withSameLocAs` n
    Ident i     -> v `withSameLocAs` i
    List l      -> v `withSameLocAs` l
    Construct c -> v `withSameLocAs` c
    String s    -> v `withSameLocAs` s
