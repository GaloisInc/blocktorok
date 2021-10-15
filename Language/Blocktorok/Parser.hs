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
    -- ** Parsing 'BlockElement's
    elementsFromFile
    -- ** Raw entry
  , parseBlocktorok
  ) where

import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Text.IO                 as TextIO
import           Data.Void                    (Void)

import           Text.Megaparsec              ((<|>))
import qualified Text.Megaparsec              as MP
import           Text.Megaparsec.Char         (char)
import qualified Text.Megaparsec.Char.Lexer   as Lexer

import           Language.Blocktorok.Syntax   (BlockElement (..), Value (..))
import           Language.Common              (Located (..))
import           Language.Common.Parser       (brackets, lexeme, lident,
                                               located, optional, spc, symbol')
import qualified Language.Common.Units.Parser as UP

type Parser a = MP.Parsec Void Text a

blockContents :: Parser [BlockElement]
blockContents = MP.many blockElement

blockElement :: Parser BlockElement
blockElement = BlockElement <$> (lident <* symbol' ":") <*> value

value :: Parser Value
value =
      blockValue
  <|> num
  <|> List <$> located (symbol' "[" *> MP.sepBy value (symbol' ",")   <* symbol' "]")
  <|> str
  <|> tag
  where
    num = Number <$> located (Lexer.signed (pure ()) $ lexeme Lexer.scientific) <*> munit
    munit = optional $ symbol' "(" *> located UP.parseUnit <* symbol' ")"

    blockValue = Block <$> brackets (located blockContents)

    tag =
      do  t <- MP.try (lident <* MP.notFollowedBy (symbol' ":"))
          val <- optional value
          pure $ Tag t val

    str =
      do  s <- located strLit
          pure $ String (Text.pack <$> s)
    strLit = char '"' >> MP.manyTill Lexer.charLiteral (char '"')

-------------------------------------------------------------------------------

parse ::  Parser a -> FilePath -> Text -> Either Text a
parse parser fp t =
  case MP.runParser parser fp t of
    Right a   -> Right a
    Left errs -> Left . Text.pack $ MP.errorBundlePretty errs

-- | Parse the given 'Text' to a 'Block'
parseBlocktorok :: Text -> Either Text [BlockElement]
parseBlocktorok = parse (spc *> blockContents <* MP.eof) "--input--"

-- | Parse many 'Block's from the contents of a given file, returning the
-- results as 'BlockElement's
elementsFromFile :: FilePath -> IO (Either Text (Located [BlockElement]))
elementsFromFile f =
  parse (spc *> located blockContents <* MP.eof) f <$> TextIO.readFile f
