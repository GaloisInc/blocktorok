{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Language.Schema.Parser
Description : Parsers for the schema language
Copyright   : (c) Galois, Inc. 2021
License     : N/A
Maintainer  : cphifer@galois.com
Stability   : experimental
Portability : N/A

Parsers for the LINK schema language, including high-level entrypoints to be
used in the glue code connecting schemas to the transformer language.
-}

module Language.Schema.Parser
  ( schemaFromFile
  ) where

import Control.Monad (void)

import Data.Char (isAlpha, isDigit)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import Data.Void (Void)

import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MPC
import qualified Text.Megaparsec.Char.Lexer as Lexer

import Language.Common (Located(..), SourceRange(..))
import Language.Schema.Syntax

type Parser a = MP.Parsec Void Text a

spc :: Parser ()
spc = Lexer.space MPC.space1
                  MP.empty
                  MP.empty

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme spc

symbol :: Text -> Parser Text
symbol = Lexer.symbol spc

symbol' :: Text -> Parser ()
symbol' t = void $ symbol t

mkRange :: MP.SourcePos -> MP.SourcePos -> SourceRange
mkRange s e =
  SourceRange (MP.sourceName s) (asLoc s) (asLoc e)
  where
    asLoc pos = (MP.unPos (MP.sourceLine pos), MP.unPos (MP.sourceColumn pos))

located :: Parser a -> Parser (Located a)
located p =
  lexeme $
    do start <- MP.getSourcePos
       a <- p
       end <- MP.getSourcePos
       pure $ Located (mkRange start end) a

optional :: Parser a -> Parser (Maybe a)
optional p = (Just <$> MP.try p) <|> pure Nothing

ident :: Parser Text
ident =
  lexeme . MP.try $
    do c0 <- MP.satisfy identFirstChar
       cr <- MP.takeWhileP Nothing identRestChar
       pure (c0 `Text.cons` cr)
  where
    identFirstChar :: Char -> Bool
    identFirstChar c = isAlpha c || c == '_'
    identRestChar c = identFirstChar c || isDigit c

brackets :: Parser a -> Parser a
brackets p = symbol' "{" *> p <* symbol' "}"

typeP :: Parser SType
typeP = int <|> float <|> string <|> list <|> union
  where
    int    = symbol' "int"    *> pure SInt
    float  = symbol' "float"  *> pure SFloat
    string = symbol' "string" *> pure SString
    list   = symbol' "list"   *> (SList <$> typeP)
    union  = SUnion <$> ident

decl :: Parser Decl
decl = Decl <$> (located ident <* symbol' ":") <*> located typeP

docAnn :: Parser Text
docAnn = Text.pack <$> (symbol "[--" *> MP.manyTill Lexer.charLiteral (symbol "--]"))

variant :: Parser Variant
variant =
  Variant <$> optional docAnn
          <*> located ident
          <*> (brackets (MP.sepBy decl (symbol' ",")) <* symbol' ";")

union :: Parser Union
union =
  Union <$> (symbol' "union" *> located ident)
        <*> brackets (MP.some variant)

-------------------------------------------------------------------------------

parseSchema :: Text -> Either Text Union
parseSchema t =
  case MP.runParser union "-input-" t of
    Right u -> Right u
    Left errs -> Left . Text.pack $ MP.errorBundlePretty errs

schemaFromFile :: FilePath -> IO (Either Text Union)
schemaFromFile fp = parseSchema <$> TIO.readFile fp
