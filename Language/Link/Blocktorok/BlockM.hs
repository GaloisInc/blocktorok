{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BlockArguments #-}

-- generate docs from this description?
module Language.Link.Blocktorok.BlockM where

import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Except as Except
import Control.Applicative(Alternative(..))
import Data.Text(Text)
import qualified Data.Text as Text

import Language.Common (Located(..), ppRange)
import Language.Link.Blocktorok.Parser(parseBlocktorok)
import Language.Link.Blocktorok.Syntax
    ( Value,
      BlockElement(..),
      Block(..),
      Value(..),
      Constructor(..),
      locateValue,
      Ident )

newtype BlockM a = BlockM { unBlockM :: Reader.ReaderT Block (Except.Except Text) a }
  deriving(Monad, Applicative, Functor)

instance Except.MonadError Text BlockM where
  throwError t = BlockM $ Except.throwError t
  catchError m f = BlockM $ Except.catchError (unBlockM m) (unBlockM . f)

instance Alternative BlockM where
  empty = Except.throwError ("empty alternative" :: Text)
  -- some way to combine errors?
  a <|> b =
    do  pa <- option a
        case pa of
          Just a' -> pure a'
          Nothing -> b

runBlockM :: Block -> BlockM a -> Either Text a
runBlockM blk cmp =
  let except = Reader.runReaderT (unBlockM cmp) blk
  in  Except.runExcept except

err :: Located a -> Text -> BlockM b
err why msg = Except.throwError (ppRange (locRange why) <> " " <> msg)

inBlock :: Block -> BlockM a -> BlockM a
inBlock blk m =
  BlockM $ Reader.withReaderT (const blk) (unBlockM m)

quote :: Text -> Text
quote t = "'" <> t <> "'"

quote' :: Located Text -> Text
quote' t = "'" <> locValue t <> "'"

option :: BlockM a -> BlockM (Maybe a)
option cmp =
  (Just <$> cmp) `Except.catchError` const (pure Nothing)

value :: Ident -> BlockM Value
value n =
  do  block <- BlockM Reader.ask
      case values block of
        [] -> err (blockTypeName block) ("key " <> quote n <> " not found in block " <> quote' (blockTypeName block) )
        [(_,v)] -> pure v
        (m,_):_ -> err m (quote n <> " is used as a key multiple times in block " <> quote' (blockTypeName block))
  where
    values block = [(m,v) | (BlockValue m v) <- blockContents block
                          , locValue m == n
                          ]

subBlock :: Ident -> BlockM a -> BlockM a
subBlock i m =
  do  as <- subBlocks i m
      block <- BlockM Reader.ask
      case as of
        [] -> err (blockTypeName block) ("sub block of type " <> quote i <> " not found in block " <> quote' (blockTypeName block))
        [b] -> pure b
        _ -> err (blockTypeName block) ("sub blocks of type " <> quote i <> " appear multiple times in block" <> quote' (blockTypeName block)
                                                                 <> " but should appear once")

subBlocks :: Ident -> BlockM a -> BlockM [a]
subBlocks i f =
  do  block <- BlockM Reader.ask
      flip inBlock f `traverse` sb block
  where
    sb block = [locValue b | BlockSub b <- blockContents block
                           , locValue (blockTypeName (locValue b)) == i
                           ]

valueOf :: Ident -> ValueSpec a -> BlockM a
valueOf i vs = value i >>= vs

type ValueSpec a = Value -> BlockM a

double :: ValueSpec Double
double v =
  case v of
    Number n -> pure (locValue n)
    _ -> err (locateValue v) "expecting numeric value here"

string :: ValueSpec Text
string v =
  case v of
    String s -> pure (locValue s)
    _ -> err (locateValue v) "expecting string value here"

list :: ValueSpec a -> ValueSpec [a]
list spec v =
  case v of
    List l -> spec `traverse` locValue l
    _ -> err (locateValue v) "expecting list value here"

ident :: ValueSpec Ident
ident v =
  case v of
    Ident i -> pure (locValue i)
    _ -> err (locateValue v) "expecting identifier here"


cnsToBlock :: Constructor -> Block
cnsToBlock (Constructor name fields) =
  Block name Nothing fieldsAsBlock
  where
    fieldsAsBlock = uncurry BlockValue <$> fields

cnsName :: Located Constructor -> Ident
cnsName = locValue . constructorName . locValue

constructor :: (Ident -> BlockM a) -> ValueSpec a
constructor f v =
  case v of
    Construct c -> inBlock (cnsToBlock (locValue c)) (f (cnsName c))
    _ -> err (locateValue v) "expecting constructor here"

oneConstructorOf :: [(Ident, BlockM a)] -> ValueSpec a
oneConstructorOf spec v =
  case v of
    Construct c ->
      case cnsName c `lookup` spec of
        Nothing -> expErr
        Just b -> inBlock (cnsToBlock (locValue c)) b
    _ -> expErr
  where
    cnames = Text.intercalate ", " (fst <$> spec)
    expErr = err (locateValue v) ("expecting one of the following constructors here: " <> cnames)


orElse :: BlockM a -> a -> BlockM a
orElse b a = b <|> pure a

----
-- basic tests -- move to test project

runTest :: Text -> BlockM a -> Either Text a
runTest src prg = parseBlocktorok src >>= (`runBlockM` prg)


test1 :: Either Text (Double, Maybe Double, Double, Double, Double, [Double], Text)
test1 = runTest block prg
  where
    prg =
      do  y <- option $ valueOf "y" double
          x <- valueOf "x" double
          coords <- valueOf "coords" (list double)
          desc <- valueOf "desc" string
          cns_i <- valueOf "cns" $ oneConstructorOf [("Hello", valueOf "i" double)
                                                    ,("Hi", valueOf "j" double)
                                                    ]

          (c_x, c_y) <- subBlock "cookies"
            do  c_x <- valueOf "x" double
                c_y <- valueOf "y" double
                pure (c_x, c_y)

          pure (x, y, c_x, c_y, cns_i, coords, desc)
    block =
      Text.pack $ unlines [ "su2 {"
                          , "  desc: \"muppet guppy\""
                          , "  x: 30.0"
                          , "  coords: [10.0, 31.0, 6.0]"
                          , "  cns: Hi { j = 18.0 }"
                          , "  cookies foo {"
                          , "     x: 10.0"
                          , "     y: 20.0"
                          , "  }"
                          , "}"
                          ]



