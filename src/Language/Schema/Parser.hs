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
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (State)
import qualified Control.Monad.State as State

import Data.Char (isAlpha, isDigit, isUpper)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import Data.Void (Void)

import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MPC
import qualified Text.Megaparsec.Char.Lexer as Lexer

import Language.Common.Located (Located(..), SourceRange(..))
import Language.Common.Units.Parser
import Language.Common.Units.SI
import Language.Common.Units.SymbolTable
import Language.Schema.Env
  ( Env
  , addRootType
  , addTypeDef
  , emptyEnv
  , lookupTypeDef
  )
import Language.Schema.Syntax
import Language.Schema.Type
  ( Ident
  , SType(..)
  , Globbed(..)
  , containedName
  , containsNamed
  , unGlob
  )

type Parser a = MP.ParsecT Void Text (ReaderT SymbolTable (State Env)) a

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
    int    = symbol' "int"    *> (SInt <$> parseUnit)
    float  = symbol' "float"  *> (SFloat <$> parseUnit)
    i      = symbol' "ident"  *> pure SIdent
    string = symbol' "string" *> pure SString
    list   = symbol' "list"   *> (SList <$> stype)
    named  = SNamed <$> ident

decl :: Parser Ident -> Parser Decl
decl p =
  do n <- located p
     symbol' ":"
     t <- located stype
     if containsNamed (locValue t) then
       do let nm = containedName (locValue t)
          env <- State.get
          case lookupTypeDef nm env of
            Nothing -> fail $ "The type " ++ show nm ++ " is not defined."
            Just _ -> pure $ Decl n t
     else pure $ Decl n t

doc :: Parser Text
doc = Text.pack <$> (symbol' "[--" *> MP.manyTill Lexer.charLiteral (symbol' "--]"))

-- | TODO: Better detection of / error for duplicate fields?
variant :: Parser Variant
variant =
  do ann   <- optional (located doc)
     t     <- located tag
     decls <- brackets $ MP.sepBy (decl ident) (symbol' ",")
     symbol' ";"
     if length (declNames decls) /= length (List.nub (declNames decls)) then
       fail "The preceding union variant contains duplicated field names."
     else
       pure $ Variant ann t (declsMap decls)
  where
    declNames = fmap (locValue . declName)

-- | TODO: Better detection of / error for duplicate tags?
union :: Parser Union
union =
  do symbol' "union"
     nm   <- located ident
     vars <- brackets $ MP.some variant
     if length (varNames vars) /= length (List.nub (varNames vars)) then
       fail "The preceding union definition contains duplicated variant tags."
     else
       do let u = Union nm (variantsMap vars)
          addTypeDef (locValue nm) (UnionDef u)
          pure u
  where
    varNames = fmap (locValue . variantTag)

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
  do symbol' "block"
     t  <- located ident
     nm <- optional (located selector)
     fs <- brackets $ MP.many $ globbed blockDecl
     if length (fieldNames fs) /= length (List.nub (fieldNames fs)) then
       fail "The preceding block definition contains duplicated field names."
     else
       do let b = BlockS t nm (globbedDeclsMap fs)
          addTypeDef (locValue t) (BlockDef b)
          pure b
  where
    fieldNames = fmap (locValue . declName . blockDeclDecl . unGlob)

-- blockExt :: Parser BlockS
-- blockExt =
--   do symbol' "block"
--      new <- located ident
--      symbol' "extends"
--      old <- located ident
--      fs <- brackets $ MP.many $ globbed blockDecl


root :: Parser Root
root =
  do symbol' "root"
     fs <- brackets $ MP.some $ globbed blockDecl
     if length (fieldNames fs) /= length (List.nub (fieldNames fs)) then
       fail "The root specifiation contains duplicated field names."
     else
       do sequence_ (addRootType <$> fs)
          pure $ Root $ globbedDeclsMap fs
  where
    fieldNames = fmap (locValue . declName . blockDeclDecl . unGlob)
  -- Root <$> (symbol' "root" *> brackets (globbedDeclsMap <$> (MP.some $ globbed blockDecl)))

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
  case State.evalState (flip runReaderT siTable $ MP.runParserT schema fp t) emptyEnv of
    Right u -> Right u
    Left errs -> Left . Text.pack $ MP.errorBundlePretty errs

schemaFromFile :: FilePath -> IO (Either Text Schema)
schemaFromFile fp = parseSchema fp <$> TIO.readFile fp
