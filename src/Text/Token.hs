{-|
Module      : Text.Token
Description : Interface between Alex lexer and Parsec parser
Copyright   : Galois, Inc. 2021
License     : N/A
Maintainer  : cphifer@galois.com
Stability   : Experimental
Portability : N/A

This module defines an interface between the lexer, generated using Alex, and
Parsec, which is used to parse LINK programs.
-}

{-# LANGUAGE FlexibleContexts, MonoLocalBinds #-}

module Text.Token
  ( tok
  , tok'
  , floating
  , integer
  , satisfy'
  , variable
  , Parser
  ) where

import Control.Monad.Reader (Reader, void)

import Data.Units.SymbolTable (SymbolTable)

import Language.Haskell.TH.Syntax (Name)

import Text.Lexer (Token(..), AlexPosn(..))
import Text.TokenClass (TokenClass(..))

import Text.Parsec hiding (satisfy)

-- | The type of LINK parsers.
type Parser = ParsecT [Token] () (Reader (SymbolTable Name Name))

satisfy :: (Stream [Token] m Token) => (Token -> Bool) -> ParsecT [Token] u m TokenClass
satisfy f = tokenPrim show nextPos tokeq
  where
    tokeq :: Token -> Maybe TokenClass
    tokeq t@(Token _ tc) = if f t then Just tc else Nothing

-- | Parse a token based on a predicate returning a 'Maybe' value.
satisfy' :: (Stream [Token] m Token) => (Token -> Maybe a) -> ParsecT [Token] u m a
satisfy' = tokenPrim show nextPos

nextPos :: SourcePos -> Token -> [Token] -> SourcePos
nextPos pos _ ((Token (AlexPn _ l c) _):_) = setSourceColumn (setSourceLine pos l) c
nextPos pos _ []                           = pos

-- | Parse the given 'TokenClass'.
tok :: (Stream [Token] m Token) => TokenClass -> ParsecT [Token] u m TokenClass
tok t = satisfy (\(Token _ t') -> t' == t) <?> show t

-- | Parse (and ignore) the given 'TokenClass'.
tok' :: (Stream [Token] m Token) => TokenClass -> ParsecT [Token] u m ()
tok' p = void $ tok p

-- | Parse a @TokenFloat@.
floating :: Monad m => ParsecT [Token] u m Double
floating = satisfy' p <?> "float"
  where p (Token _ t) = case t of
                          TokenFloat d -> Just d
                          _ -> Nothing

-- | Parse a @TokenInt@.
integer :: Monad m => ParsecT [Token] u m Integer
integer = satisfy' p <?> "integer"
  where p (Token _ t) = case t of
                          TokenInt i -> Just i
                          _ -> Nothing

-- | Parse a @TokenVar@.
variable :: Monad m => ParsecT [Token] u m String
variable = satisfy' p <?> "variable"
  where p (Token _ t) = case t of
                          TokenVar s -> Just s
                          _ -> Nothing
