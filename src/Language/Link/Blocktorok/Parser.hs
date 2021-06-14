{-# LANGUAGE OverloadedStrings #-}
module Language.Link.Blocktorok.Parser where

import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Control.Monad(void)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MPC
import Text.Megaparsec((<|>))
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Text.Megaparsec.Char(char)
import Data.Char(isAlpha, isDigit)
import Data.Void(Void)
import qualified Data.Scientific as SciN

import Language.Link.Blocktorok.Syntax
    ( Value(..), BlockElement(..), Block(Block), Located(..), SourceRange(..), Constructor(..) )

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
  <|> Number <$> located (SciN.toRealFloat <$> lexeme Lexer.scientific)  -- use 'scientific instead?
  <|> List <$> located (symbol' "[" *> MP.sepBy value (symbol' ",")   <* symbol' "]")
  <|> str
  <|> Ident <$> located ident
  where
    cns = Constructor <$> (located ident <* symbol' "{") <*> (termBy nv (symbol ",") <* symbol "}")
    str =
      do  s <- located strLit
          pure $ String (Text.pack <$> s)

    strLit = char '"' >> MP.manyTill Lexer.charLiteral (char '"')
    nv =
      do  n <- located ident
          symbol' "="
          v <- value
          pure (n,v)

-------------------------------------------------------------------------------

parseBlocktorok :: Text -> Either Text Block
parseBlocktorok t =
  case MP.runParser block "-input-" t of
    Right blk -> Right blk
    Left errs -> Left . Text.pack $ MP.errorBundlePretty errs

blockFromFile :: FilePath -> IO (Either Text Block)
blockFromFile fp = parseBlocktorok <$> TextIO.readFile fp