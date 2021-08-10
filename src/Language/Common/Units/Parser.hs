{-|
Module      : Language.Common.Units.Parser
Description : Parsing of SI units
Copyright   : (c) Galois, Inc. 2021
License     : N/A
Maintainer  : cphifer@galois.com
Stability   : experimental
Portability : N/A

Parsers for units to be used in the schema/transformer languages.
-}

module Language.Common.Units.Parser where

import Control.Monad (void)

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import Data.Void (Void)

import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MPC
import qualified Text.Megaparsec.Char.Lexer as Lexer

import Language.Common.Located (Located(..), SourceRange(..))

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
