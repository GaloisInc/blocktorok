{-|
Module      : Language.Parser
Description : Parser for the LINK language
Copyright   : Galois, Inc. 2021
License     : N/A
Maintainer  : cphifer@galois.com
Stability   : Experimental
Portability : N/A

This module defines the parser for the LINK language, using Parsec.
-}

module Language.Parser
  ( parseDecl
  ) where

import Language.AST
import Language.Lexer
import Language.Token

import Text.Parsec

parseText :: Parser a -> String -> Either ParseError a
parseText p = parseNamedText p "<string>"

parseNamedText :: Parser a -> String -> String -> Either ParseError a
parseNamedText p n s =
  case llex s of
    Left e -> parse (reportLexError e) n []
    Right xs -> parse p n xs

reportLexError :: String -> Parser a
reportLexError msg = fail ("lexical error: " ++ msg)

parseDecl :: FilePath -> String -> Either String Prog
parseDecl = undefined
