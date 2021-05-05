{-# LANGUAGE OverloadedStrings #-}
module Language.Link.Blocktorok where

import Data.Text(Text)
import qualified Data.Text as Text
import Control.Monad(void)
import qualified Text.Megaparsec as MP
import Text.Megaparsec((<|>))
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Text.Megaparsec.Char(space, char)
import Data.Char(isAlpha, isDigit)

data Block =
  Block { blockTypeName :: Text
        , blockName :: Maybe Text
        , blockContents :: [BlockElement]
        }

data BlockElement =
    BlockSub Block
  | BlockValue Text Value

data Value =
    Number Double
  | Ident Text
  | List [Value]
  | Construct Text [(Text, Value)]
  | String Text

-------------------------------------------------------------------------------

type Parser a = MP.Parsec () Text a

consumeSpacesAndComments :: Parser ()
consumeSpacesAndComments = MP.skipMany $ Lexer.skipLineComment "--" >> space

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme consumeSpacesAndComments

symbol :: Text -> Parser Text
symbol = Lexer.symbol consumeSpacesAndComments

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
  Block <$> ident
        <*> optional ident
        <*> brackets (MP.many blockElement)

blockElement :: Parser BlockElement
blockElement = blockValue <|> (BlockSub <$> block)
  where
    blockValue =
      BlockValue <$> MP.try (ident <* symbol' ":") <*> value

termBy :: Parser a -> Parser sep -> Parser [a]
termBy elt sep =
  MP.sepBy elt sep <* optional sep

value :: Parser Value
value =
      Construct <$> MP.try (ident <* symbol' "{") <*> (termBy nv (symbol ",") <* symbol "}")
  <|> Number <$> Lexer.decimal  -- use 'scientific instead?
  <|> List <$> (symbol' "[" *> MP.many value <* symbol' "]")
  <|> String . Text.pack <$> strLit
  <|> Ident <$> ident
  where
    strLit = char '"' >> MP.manyTill Lexer.charLiteral (char '"')
    nv =
      do  n <- ident
          symbol' "="
          v <- value
          pure (n,v)

-------------------------------------------------------------------------------
