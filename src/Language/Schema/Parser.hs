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
import Control.Monad.State (State)
import qualified Control.Monad.State as State

import Data.Char (isAlpha, isDigit, isUpper)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import Data.Void (Void)

import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MPC
import qualified Text.Megaparsec.Char.Lexer as Lexer

import Language.Common (Located(..), SourceRange(..), withSameLocAs)
import Language.Schema.Env (Env, emptyEnv)
import Language.Schema.Syntax
import Language.Schema.Type (Ident, SType(..), Globbed(..))

type Parser a = MP.ParsecT Void Text (State Env) a

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
    do  start <- MP.getSourcePos
        a <- p
        end <- MP.getSourcePos
        pure $ Located (mkRange start end) a

optional :: Parser a -> Parser (Maybe a)
optional p = (Just <$> MP.try p) <|> pure Nothing

ident :: Parser Ident
ident =
  lexeme . MP.try $
    do c0 <- MP.satisfy identFirstChar
       cr <- MP.takeWhileP Nothing identRestChar
       pure (c0 `Text.cons` cr)
  where
    identFirstChar :: Char -> Bool
    identFirstChar c = isAlpha c || c == '_'
    identRestChar c = identFirstChar c || isDigit c

selector :: Parser Ident
selector = MPC.char '.' *> ident

tag :: Parser Ident
tag =
  lexeme .MP.try $
    do c0 <- MP.satisfy isUpper
       cr <- MP.takeWhileP Nothing isAlpha
       pure (c0 `Text.cons` cr)

brackets :: Parser a -> Parser a
brackets p = symbol' "{" *> p <* symbol' "}"

stype :: Parser SType
stype = int <|> float <|> i <|> string <|> list <|> named
  where
    int    = symbol' "int"    *> pure SInt
    float  = symbol' "float"  *> pure SFloat
    i      = symbol' "ident"  *> pure SIdent
    string = symbol' "string" *> pure SString
    list   = symbol' "list"   *> (SList <$> stype)
    named  = SNamed <$> ident

decl :: Parser Ident -> Parser Decl
decl p = Decl <$> (located p <* symbol' ":") <*> located stype

doc :: Parser Text
doc = Text.pack <$> (symbol' "[--" *> MP.manyTill Lexer.charLiteral (symbol' "--]"))

variant :: Parser Variant
variant =
  Variant <$> optional (located doc)
          <*> located tag
          <*> brackets (declsMap <$> MP.sepBy (decl ident) (symbol' ",")) <* symbol' ";"

union :: Parser Union
union =
  Union <$> (symbol' "union" *> located ident)
        <*> brackets (variantsMap <$> MP.some variant)

glob :: Parser (a -> Globbed a)
glob = MP.option One $ MP.choice [opt, some, many]
  where
    opt  = symbol' "?" *> pure Optional
    some = symbol' "+" *> pure Some
    many = symbol' "*" *> pure Many

globbed :: Parser a -> Parser (Globbed a)
globbed p =
  do a <- p
     g <- glob
     pure $ g a

blockDecl :: Parser BlockDecl
blockDecl =
  BlockDecl <$> optional (located doc)
            <*> decl selector

blockS :: Parser BlockS
blockS =
  BlockS <$> (symbol' "block" *> located ident)
         <*> optional (MP.try (decl selector) <|> onlySelector)
         <*> brackets (globbedDeclsMap <$> (MP.many $ globbed blockDecl))
  where
    onlySelector :: Parser Decl
    onlySelector =
      do s <- located selector
         pure $ Decl s (SIdent `withSameLocAs` s)

root :: Parser Root
root =
  Root <$> (symbol' "root" *> brackets (globbedDeclsMap <$> (MP.some $ globbed blockDecl)))

schemaDefsP :: Parser SchemaDef
schemaDefsP =  UnionDef <$> union
           <|> BlockDef <$> blockS

schema :: Parser Schema
schema =
  Schema <$> (schemaDefMap <$> MP.many schemaDefsP)
         <*> root

-------------------------------------------------------------------------------

parseSchema :: FilePath -> Text -> Either Text Schema
parseSchema fp t =
  case State.evalState (MP.runParserT schema fp t) emptyEnv of
    Right u -> Right u
    Left errs -> Left . Text.pack $ MP.errorBundlePretty errs

schemaFromFile :: FilePath -> IO (Either Text Schema)
schemaFromFile fp = parseSchema fp <$> TIO.readFile fp
