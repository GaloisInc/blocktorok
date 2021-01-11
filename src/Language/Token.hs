{-|
Module      : Language.Token
Description : Interface between Alex lexer and Parsec parser
Copyright   : Galois, Inc. 2021
License     : N/A
Maintainer  : cphifer@galois.com
Stability   : Experimental
Portability : N/A

This module defines an interface between the lexer, generated using Alex, and
Parsec, which is used to parse LINK programs.
-}

{-# LANGUAGE FlexibleContexts #-}

module Language.Token
  ( tok
  , tok'
  , number
  , variable
  , Parser
  ) where

import Control.Monad (void)

import Language.Lexer (Token(..), AlexPosn(..))
import Language.TokenClass

import Text.Parsec hiding (satisfy)

type Parser = Parsec [Token] ()

satisfy :: (Stream [Token] m Token) => (Token -> Bool) -> ParsecT [Token] u m TokenClass
satisfy f = tokenPrim show nextPos tokeq
  where
    tokeq :: Token -> Maybe TokenClass
    tokeq t@(Token _ tc) = if f t then Just tc else Nothing

satisfy' :: (Stream [Token] m Token) => (Token -> Maybe a) -> ParsecT [Token] u m a
satisfy' = tokenPrim show nextPos

nextPos :: SourcePos -> Token -> [Token] -> SourcePos
nextPos pos _ ((Token (AlexPn _ l c) _):_) = setSourceColumn (setSourceLine pos l) c
nextPos pos _ []                           = pos

tok :: (Stream [Token] m Token) => TokenClass -> ParsecT [Token] u m TokenClass
tok t = satisfy (\(Token _ t') -> t' == t) <?> show t

tok' :: (Stream [Token] m Token) => TokenClass -> ParsecT [Token] u m ()
tok' p = void $ tok p

number :: Monad m => ParsecT [Token] u m Int
number = satisfy' p <?> "integer"
  where p (Token _ t) = case t of
                          TokenInt n -> Just n
                          _ -> Nothing

variable :: Monad m => ParsecT [Token] u m String
variable = satisfy' p <?> "variable"
  where p (Token _ t) = case t of
                          TokenVar s -> Just s
                          _ -> Nothing
