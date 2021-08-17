{-# LANGUAGE OverloadedStrings #-}
module Language.Blocktorok.Syntax where

import Data.Text(Text)

import Language.Common(Located, withSameLocAs)


-------------------------------------------------------------------------------
-- AST

type Ident = Text

data Block = Block
  { blockTypeName :: Located Ident
  , blockName :: Maybe (Located Ident)
  , blockContents :: [BlockElement]
  }
  deriving(Show, Eq, Ord)

data BlockElement =
    BlockSub (Located Block)
  | BlockValue (Located Ident) Value
  deriving(Show, Eq, Ord)

data Value =
    Number (Located Double)
  | Ident (Located Ident)
  | List (Located [Value])
  | Construct (Located Constructor)
  | String (Located Text)
  deriving(Show, Eq, Ord)

data Constructor = Constructor
  { constructorName :: Located Ident
  , constructorFields :: [(Located Ident, Value)]
  }
  deriving(Show, Eq, Ord)

locateValue :: Value -> Located Value
locateValue v =
  case v of
    Number n -> v `withSameLocAs` n
    Ident i -> v `withSameLocAs` i
    List l -> v `withSameLocAs` l
    Construct c -> v `withSameLocAs` c
    String s -> v `withSameLocAs` s
