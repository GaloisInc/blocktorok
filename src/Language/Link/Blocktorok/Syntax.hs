{-# LANGUAGE OverloadedStrings #-}
module Language.Link.Blocktorok.Syntax where

import Data.Text(Text, pack)


-------------------------------------------------------------------------------
-- AST

type Ident = Text

data SourceRange = SourceRange
  { sourceRangeFile :: FilePath
  , sourceRangeStart :: (Int, Int)
  , sourceRangeEnd :: (Int, Int)
  }
  deriving(Show, Eq, Ord)

data Located a = Located
  { locRange :: SourceRange
  , locValue :: a
  }
  deriving(Show, Eq, Ord)

withSameLocAs :: b -> Located a -> Located b
withSameLocAs b a = Located (locRange a) b

ppRange :: SourceRange -> Text
ppRange (SourceRange path (startLn, startCol) (endLn, endCol)) =
  pack $
    path ++ ":" ++ show startLn ++ ":" ++ show startCol ++ "-"
                ++ show endLn ++ ":" ++ show endCol

instance Functor Located where
  fmap f a = a { locValue = f (locValue a) }

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



