module Language.Transform.Syntax where

import Data.Text(Text)
import Language.Common(HasLocation(..), Located(..), sourceRangeSpan')

type LIdent = Located Text

data Selector =
    SelName LIdent
  | SelMem  Selector LIdent
  deriving(Show, Eq, Ord)

instance HasLocation Selector where
  location s =
    case s of
      SelName n -> location n
      SelMem s' l -> sourceRangeSpan' s' l

data Expr =
    ExprFn (Located Call)
  | ExprSelector Selector
  | ExprLit Lit
  deriving(Show, Eq, Ord)

data Call = Call FName (Located [Expr])
  deriving(Show, Eq, Ord)

instance HasLocation Expr where
  location e =
    case e of
      ExprFn c -> location c
      ExprSelector s -> location s
      ExprLit l -> location l

-- TODO: units
data Lit =
    LitString (Located Text)
  | LitInt (Located Integer)
  | LitFloat (Located Double)
  deriving(Show, Eq, Ord)

instance HasLocation Lit where
  location l =
    case l of
      LitString r -> location r
      LitInt r  -> location r
      LitFloat r  -> location r


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
  deriving(Show, Eq, Ord)

data Decl =
  -- | Describe how to render a selector
    DeclRender Selector Expr

  -- | Declare a symbol
  | DeclLet LIdent Expr

  -- | Output to file
  | DeclFileOut LIdent Expr
  deriving(Show, Eq, Ord)

instance HasLocation Decl where
  location d =
    case d of
      DeclRender s e -> sourceRangeSpan' s e
      DeclLet  l e -> sourceRangeSpan' l e
      DeclFileOut f o -> sourceRangeSpan' f o

data Transform = Transform
  { transformSchema :: Located Text
  , transformDecls :: [Decl]
  }
  deriving(Show, Eq, Ord)

-------------------------------------------------------------------------------

