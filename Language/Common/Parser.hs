{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Language.Common.Parser
Description : Parsers common to all languages
Copyright   : (c) Galois, Inc. 2021
License     : N/A
Maintainer  : cphifer@galois.com
Stability   : experimental
Portability : N/A

Simple parsers and parser combinators common to all Blocktorok languages (data,
schema, transformer).
-}

module Language.Common.Parser
  ( -- * Common Blocktorok parsers
    -- ** Lexing
    ident
  , keyword
  , lexeme
  , lident
  , spc
  , symbol
  , symbol'
    -- ** Combinators
  , brackets
  , located
  , located'
  , optional
  ) where

import           Control.Monad              (void)

import           Data.Char                  (isAlpha, isDigit)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text

import           Text.Megaparsec            (ParsecT, (<?>), (<|>))
import qualified Text.Megaparsec            as MP
import qualified Text.Megaparsec.Char       as MPC
import qualified Text.Megaparsec.Char.Lexer as Lexer

import           Language.Common            (Located (..), SourceRange (..))

spc :: Ord e => ParsecT e Text m ()
spc = Lexer.space MPC.space1
                  (Lexer.skipLineComment "--")
                  MP.empty

lexeme :: Ord e => ParsecT e Text m a -> ParsecT e Text m a
lexeme = Lexer.lexeme spc

symbol :: Ord e => Text -> ParsecT e Text m Text
symbol = Lexer.symbol spc

symbol' :: Ord e => Text -> ParsecT e Text m ()
symbol' = void . symbol

optional :: Ord e => ParsecT e Text m a -> ParsecT e Text m (Maybe a)
optional p = (Just <$> p) <|> pure Nothing

located' :: Ord e => ParsecT e Text m a -> ParsecT e Text m (Located a)
located' p =
  do start <- MP.getSourcePos
     a <- p
     end <- MP.getSourcePos
     pure $ Located (mkRange start end) a
  where
    mkRange :: MP.SourcePos -> MP.SourcePos -> SourceRange
    mkRange s e =
      SourceRange (MP.sourceName s) (asLoc s) (asLoc e)

    asLoc :: MP.SourcePos -> (Int, Int)
    asLoc pos = (MP.unPos (MP.sourceLine pos), MP.unPos (MP.sourceColumn pos))

located :: Ord e => ParsecT e Text m a -> ParsecT e Text m (Located a)
located = lexeme . located'

identFirstChar :: Char -> Bool
identFirstChar c = isAlpha c || c == '_'

identRestChar :: Char -> Bool
identRestChar c = identFirstChar c || isDigit c

ident :: Ord e => ParsecT e Text m Text
ident =
  lexeme $
    do c0 <- MP.satisfy identFirstChar <?> "a letter or '_'"
       cr <- MP.takeWhileP (Just "a letter, number, or '_'") identRestChar
       pure (c0 `Text.cons` cr)

lident :: Ord e => ParsecT e Text m (Located Text)
lident = located ident

keyword :: Ord e => Text -> ParsecT e Text m ()
keyword t =
  lexeme . MP.try $
    do void $ MP.chunk t
       MP.notFollowedBy $ MP.satisfy identRestChar

brackets :: Ord e => ParsecT e Text m a -> ParsecT e Text m a
brackets p = symbol' "{" *> p <* symbol' "}"
