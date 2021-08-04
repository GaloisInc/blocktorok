module Language.Transform.Syntax where

import Data.Text(Text)
import Language.Common.Located(Located(..))

type Ident = Located Text

data Selector =
    SelName Ident
  | SelMem  Selector Ident
  deriving(Show, Eq)

data Expr =
    ExprFn FName [Located Expr]
 -- | ExprMkRecord (Map Ident (Located Expr))
  | ExprSelector Selector
  | ExprLit Lit
  deriving(Show, Eq)

-- TODO: units
data Lit =
    LitString (Located Text)
  | LitInt (Located Int)
  | LitFloat (Located Float)
  deriving(Show, Eq)

data FName =
  -- | Horizontally concat docs
    FHCat

  -- | Vertically concat docs
  | FVCat

  -- | Make a sequence
  | FMkSeq

  -- | Join docs with a separator
  | FJoin

  -- | Open a file for output
  | FFile
  deriving(Show, Eq)

data Decl =
  -- | Describe how to render a selector
    DeclRender Selector (Located Expr)

  -- | Declare a symbol
  | DeclLet Ident (Located Expr)

  -- | Output to file
  | DeclFileOut Ident (Located Expr)
  deriving(Show, Eq)

data Transform = Transform
  { transformSchema :: Located Text
  , transformDecls :: [Located Decl]
  }
  deriving(Show, Eq)

-------------------------------------------------------------------------------

