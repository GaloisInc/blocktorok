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
    -- ** Parsing 'BlockElement's
    elementsFromFile
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

import           Language.Blocktorok.Syntax (BlockElement (..), Value (..))
import           Language.Common            (Located (..), SourceRange (..))
import qualified Language.Common.Units.Parser as UP


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

blockContents :: Parser [BlockElement]
blockContents = MP.many blockElement

blockElement :: Parser BlockElement
blockElement = BlockElement <$> MP.try (located ident <* symbol' ":") <*> value

value :: Parser Value
value =
      blockValue
  <|> num
  <|> List <$> located (symbol' "[" *> MP.sepBy value (symbol' ",")   <* symbol' "]")
  <|> str
  <|> tag
  where
    num =
      do  n <- located (MP.try signedNum)
          MP.choice
            [ Quantity n <$> (MP.try  (symbol' "(") *> located UP.parseUnit <* symbol' ")")
            , pure $ Number (SciN.toRealFloat <$> n)
            ]

    blockValue =
      do  MP.try (symbol' "{")
          cts <- located blockContents
          symbol' "}"
          pure $ Block cts

    tag =
      do  t <- MP.try (located ident <* MP.notFollowedBy (symbol' ":"))
          val <- optional value
          pure $ Tag t val
    str =
      do  s <- located strLit
          pure $ String (Text.pack <$> s)
    signedNum = Lexer.signed spc $ lexeme Lexer.scientific
    strLit = MP.try (char '"') >> MP.manyTill Lexer.charLiteral (char '"')

-------------------------------------------------------------------------------

parse ::  Parser a -> FilePath -> Text -> Either Text a
parse parser fp t =
  case MP.runParser parser fp t of
    Right a   -> Right a
    Left errs -> Left . Text.pack $ MP.errorBundlePretty errs

-- | Parse the given 'Text' to a 'Block'
parseBlocktorok :: Text -> Either Text [BlockElement]
parseBlocktorok = parse blockContents "--input--"

-- | Parse many 'Block's from the contents of a given file, returning the
-- results as 'BlockElement's
elementsFromFile :: FilePath -> IO (Either Text (Located [BlockElement]))
elementsFromFile f =
  parse (located blockContents) f <$> TextIO.readFile f
