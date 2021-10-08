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
    BlockElement(..)
  , Ident
  , Value(..)
    -- ** Utility functions
  , locateValue
  ) where

import           Data.Text       (Text)

import           Language.Common (Located(..), withSameLocAs, sourceRangeSpan')

-- | Identifiers; type alias for easy representation changes in the future
type Ident = Text

-- | Data that may appear within a Blocktorok 'Block'
data BlockElement = BlockElement (Located Ident) Value
  deriving(Show, Eq, Ord)

-- | Blocktorok values, corresponding to the types fields may be declared to
-- have in a valid schema
data Value =
    Number (Located Double)
  | List (Located [Value])
  | Block (Located [BlockElement])
  | Tag (Located Ident) (Maybe Value)
  | String (Located Text)
  deriving(Show, Eq, Ord)

-- | Locate a 'Value' where its underlying data is located
locateValue :: Value -> Located Value
locateValue v =
  case v of
    Number n         -> v `withSameLocAs` n
    List l           -> v `withSameLocAs` l
    Tag i Nothing    -> v `withSameLocAs` i
    Tag i (Just val) -> Located (sourceRangeSpan' i (locateValue val)) val
    Block elts       -> v `withSameLocAs` elts
    String s         -> v `withSameLocAs` s
