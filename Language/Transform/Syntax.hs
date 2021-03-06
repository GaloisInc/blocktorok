{-|
Module      : Language.Transform.Syntax
Description : Blocktorok transformer language syntax definition
Copyright   : (c) Galois, Inc. 2021
License     : N/A
Maintainer  : james.lamar@galois.com
Stability   : experimental
Portability : N/A

Definition of the AST for Blocktorok transformers; for definitions of schema
language syntax see "Language.Schema.Syntax", and for the data language see
"Language.Blocktorok.Syntax".
-}

module Language.Transform.Syntax
  ( -- * Blocktorok Transformers
    -- ** The AST
    Call(..)
  , Decl(..)
  , Expr(..)
  , FName(..)
  , Lit(..)
  , Selector(..)
  , Transform(..)
  , SelectorElement(..)
  ) where

import           Data.Text                   (Text)

import           Language.Common             (HasLocation (..), Located (..),
                                              SourceRange, sourceRangeSpan',
                                              sourceRangeSpans)

import qualified Data.List.NonEmpty          as NEL
import           Language.Common.Units.Units (Unit)

-- | Identifiers carrying their location; type alias for easy representation
-- changes
type LIdent = Located Text

data SelectorElement =
    SelName LIdent
  | SelSchema LIdent
  | SelCond Expr
  deriving(Show, Eq, Ord)

instance HasLocation SelectorElement where
  location s =
    case s of
      SelName n   -> location n
      SelSchema l -> location l
      SelCond e   -> location e

newtype Selector = Selector { selectorElements :: NEL.NonEmpty SelectorElement }
  deriving(Show, Eq, Ord)

instance HasLocation Selector where
  location s = sourceRangeSpans (selectorElements s)

-- | Expressions in the transformer language, which can be interpolated in
-- so-called "bar strings". 'Selector' in this context refers to the data
-- that is being transformed rather than part of the schema
data Expr =
    ExprFn (Located Call)
  | ExprSelector Selector
  | ExprLit Lit
  | ExprFor LIdent Expr Expr
  | ExprCond SourceRange [(Expr, Expr)] Expr
  | ExprConvertUnits Expr (Located Unit)
  | ExprNot Expr
  | ExprAnd Expr Expr
  | ExprOr Expr Expr
  deriving(Show, Eq, Ord)

-- | A function call (e.g. @join(", ", foo.bar)@)
data Call = Call FName (Located [Expr])
  deriving(Show, Eq, Ord)

instance HasLocation Expr where
  location e =
    case e of
      ExprFn c              -> location c
      ExprSelector s        -> location s
      ExprLit l             -> location l
      ExprFor ident _ e2    -> sourceRangeSpan' ident e2
      ExprCond r _ _        -> r
      ExprConvertUnits e2 u -> sourceRangeSpan' e2 u
      ExprNot e'            -> location e'
      ExprAnd e1 e2         -> sourceRangeSpan' e1 e2
      ExprOr e1 e2          -> sourceRangeSpan' e1 e2

-- | Literals in the transformer language
data Lit =
    LitString (Located Text)
  | LitInt (Located Integer)
  | LitFloat (Located Double)
  deriving(Show, Eq, Ord)

instance HasLocation Lit where
  location l =
    case l of
      LitString r -> location r
      LitInt r    -> location r
      LitFloat r  -> location r

-- | Primitive functions provided in the transformer language. Only @join@ and
-- @file@ are exposed to the user; the others are internally computed to
-- support bar strings
data FName =
  -- | Horizontally concat docs
    FHCat

  -- | Vertically concat docs
  | FVCat

  -- | Make a sequence
  | FMkSeq

  -- | Join docs horizontally with a separator
  | FJoin

  -- | Join docs vertically with some number of newlines
  | FVJoin

  -- | Open a file for output
  | FFile

  -- | Is a list/selector empty?
  | FIsEmpty

  -- | Boolean negation
  | FNot
  deriving(Show, Eq, Ord)

-- | Top-level declarations defining a transformer, consisting of:
--
-- * Rendering instructions - here, a 'Selector' refers to overall data shape
-- as defined in the data schema
-- * Variable definitions
-- * Writing output to files
data Decl =
  -- | Describe how to render a selector
    DeclRender Selector Expr

  -- | Declare a symbol
  | DeclLet LIdent Expr

  -- | Output to file
  | DeclFileOut LIdent Expr

  -- | Subtemplates
  | DeclIn SourceRange Selector [Decl]

  | DeclRequire Expr (Located Text)
  deriving(Show, Eq, Ord)

instance HasLocation Decl where
  location d =
    case d of
      DeclRender s e  -> sourceRangeSpan' s e
      DeclLet  l e    -> sourceRangeSpan' l e
      DeclFileOut f o -> sourceRangeSpan' f o
      DeclRequire e s -> sourceRangeSpan' e s
      DeclIn r _ _    -> r

-- | A full transformer, consisting of a schema filename and the 'Decl's which
-- define the transformation
data Transform = Transform
  { transformSchema :: Located Text
  , transformDecls  :: [Decl]
  }
  deriving(Show, Eq, Ord)

-------------------------------------------------------------------------------

