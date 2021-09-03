{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Language.Blocktorok.Parser
Description : Parser for the Blocktorok data language
Copyright   : (c) Galois, Inc. 2021
License     : N/A
Maintainer  : james.lamar@galois.com
Stability   : experimental
Portability : N/A

Parsing of Blocktorok data, including a raw entrypoint and conveniences for
parsing a 'Block', many 'Block's, and 'BlockElement's from files.
-}

module Language.Blocktorok.Parser
  ( -- * Parsing Blocktorok data
    -- ** Parsing 'Block's
    blockFromFile
  , blocksFromFile
    -- ** Parsing 'BlockElement's
  , elementsFromFile
    -- ** Raw entry
  , parseBlocktorok
  ) where

import           Control.Monad              (void)

import           Data.Char                  (isAlpha, isDigit)
import qualified Data.Scientific            as SciN
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.IO               as TextIO
import           Data.Void                  (Void)

import           Text.Megaparsec            ((<|>))
import qualified Text.Megaparsec            as MP
import           Text.Megaparsec.Char       (char)
import qualified Text.Megaparsec.Char       as MPC
import qualified Text.Megaparsec.Char.Lexer as Lexer

import           Language.Blocktorok.Syntax (Block (Block), BlockElement (..),
                                             Constructor (..), Value (..))
import           Language.Common            (Located (..), SourceRange (..))

type Parser a = MP.Parsec Void Text a

spc :: Parser ()
spc = Lexer.space MPC.space1
                  (Lexer.skipLineComment "--")
                  MP.empty

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme spc

symbol :: Text -> Parser Text
symbol = Lexer.symbol spc

mkRange :: MP.SourcePos -> MP.SourcePos -> SourceRange
mkRange s e =
  SourceRange (MP.sourceName s) (asLoc s) (asLoc e)
  where
    asLoc pos = (MP.unPos (MP.sourceLine pos), MP.unPos (MP.sourceColumn pos))

located :: Parser a -> Parser (Located a)
located p =
  lexeme $
    do  start <- MP.getSourcePos
        a <- p
        end <- MP.getSourcePos
        pure $ Located (mkRange start end) a

symbol' :: Text -> Parser ()
symbol' t = void $ symbol t

optional :: Parser a -> Parser (Maybe a)
optional p = (Just <$> MP.try p) <|> pure Nothing

ident :: Parser Text
ident =
  lexeme . MP.try $
    do  c0 <- MP.satisfy identFirstChar
        cr <- MP.takeWhileP Nothing identRestChar
        pure (c0 `Text.cons` cr)
  where
    identFirstChar :: Char -> Bool
    identFirstChar c = isAlpha c || c == '_'
    identRestChar c = identFirstChar c || isDigit c

brackets :: Parser a -> Parser a
brackets p = symbol "{" *> p <* symbol "}"

block :: Parser Block
block =
  Block <$> located ident
        <*> optional (located ident)
        <*> brackets (MP.many blockElement)

blockElement :: Parser BlockElement
blockElement = blockValue <|> (BlockSub <$> located block)
  where
    blockValue =
      BlockValue <$> MP.try (located ident <* symbol' ":") <*> value

termBy :: Parser a -> Parser sep -> Parser [a]
termBy elt sep =
  MP.sepBy elt sep <* optional sep

value :: Parser Value
value =
      Construct <$> MP.try (located cns)
  <|> Number <$> located (SciN.toRealFloat <$> signedNum)  -- use 'scientific instead?
  <|> List <$> located (symbol' "[" *> MP.sepBy value (symbol' ",")   <* symbol' "]")
  <|> str
  where
    cns = Constructor <$> (located ident <* symbol' "{") <*> (termBy nv (symbol ",") <* symbol "}")
    str =
      do  s <- located strLit
          pure $ String (Text.pack <$> s)
    signedNum = Lexer.signed spc $ lexeme Lexer.scientific
    strLit = char '"' >> MP.manyTill Lexer.charLiteral (char '"')
    nv =
      do  n <- located ident
          symbol' "="
          v <- value
          pure (n,v)

-------------------------------------------------------------------------------

parse ::  Parser a -> FilePath -> Text -> Either Text a
parse parser fp t =
  case MP.runParser parser fp t of
    Right a   -> Right a
    Left errs -> Left . Text.pack $ MP.errorBundlePretty errs

-- | Parse the given 'Text' to a 'Block'
parseBlocktorok :: Text -> Either Text Block
parseBlocktorok = parse block "--input--"

-- | Parse a 'Block' from the contents of a given file
blockFromFile :: FilePath -> IO (Either Text Block)
blockFromFile fp = parse block fp <$> TextIO.readFile fp

-- | Parse many 'Block's from the contents of a given file
blocksFromFile :: FilePath -> IO (Either Text [Block])
blocksFromFile fp = parse (MP.many block) fp <$> TextIO.readFile fp

-- | Parse many 'Block's from the contents of a given file, returning the
-- results as 'BlockElement's
elementsFromFile :: FilePath -> IO (Either Text (Located [BlockElement]))
elementsFromFile f =
  parse (located $ MP.many (BlockSub <$> located block)) f <$> TextIO.readFile f
