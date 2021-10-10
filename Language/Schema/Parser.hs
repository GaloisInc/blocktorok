{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Language.Schema.Parser
Description : Parsers for the schema language
Copyright   : (c) Galois, Inc. 2021
License     : N/A
Maintainer  : cphifer@galois.com
Stability   : experimental
Portability : N/A

Parsers for the Blocktorok schema language, including high-level entrypoints to
be used in the glue code connecting schemas to the transformer language.
-}

module Language.Schema.Parser
  ( -- * Parsing Blocktorok schemas
    schemaASTFromFile
  , schemaEnvFromFile
  ) where

import           Control.Monad              (void)
import           Control.Monad.State        (State)
import qualified Control.Monad.State        as State

import           Data.Char                  (isAlpha, isDigit)
import           Data.Functor               (($>))
import qualified Data.List                  as List
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.IO               as TIO
import           Data.Void                  (Void)

import           Text.Megaparsec            ((<|>))
import qualified Text.Megaparsec            as MP
import qualified Text.Megaparsec.Char       as MPC
import qualified Text.Megaparsec.Char.Lexer as Lexer

import           Language.Common            (Located (..), SourceRange (..))
import           Language.Schema.Env        (Env, addRootType, addTypeDef,
                                             emptyEnv, lookupTypeDef)
import           Language.Schema.Syntax     (BlockDecl (BlockDecl, blockDeclDecl),
                                             BlockS (BlockS),
                                             Decl (Decl, declName), Root (Root),
                                             Schema (Schema), SchemaDef (..),
                                             Union (Union),
                                             Variant (Variant, variantTag),
                                             globbedDeclsMap, schemaDefMap,
                                             variantsMap)
import           Language.Schema.Type       (Globbed (..), Ident, SType (..),
                                             containedName, containsNamed,
                                             unGlob)

type Parser a = MP.ParsecT Void Text (State Env) a

spc :: Parser ()
spc = Lexer.space MPC.space1
                  (Lexer.skipLineComment "--")
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

brackets :: Parser a -> Parser a
brackets p = symbol' "{" *> p <* symbol' "}"

stype :: Parser SType
stype =
  do tyName <- ident
     case tyName of
       "int" -> pure SInt
       "bool" -> pure SBool
       "float" -> pure SFloat
       "string" -> pure SString
       _ -> pure $ SNamed tyName

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
            Just _  -> pure $ Decl n t
     else pure $ Decl n t

doc :: Parser Text
doc = Text.pack <$> (symbol' "[-- " *> MP.manyTill Lexer.charLiteral (symbol' " --]"))

-- | TODO: Better detection of / error for duplicate fields?
variant :: Parser Variant
variant =
  do ann   <- optional (located doc)
     t     <- located ident
     ty    <- optional stype
     symbol' ";"
     pure $ Variant ann t ty

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
    opt  = symbol' "?" $> Optional
    some = symbol' "+" $> Some
    many = symbol' "*" $> Many

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
     fs <- brackets $ MP.many $ globbed blockDecl
     if length (fieldNames fs) /= length (List.nub (fieldNames fs)) then
       fail "The preceding block definition contains duplicated field names."
     else
       do let b = BlockS t (globbedDeclsMap fs)
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
     fs <- brackets $ MP.many $ globbed blockDecl
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
schema = spc *> (Schema <$> (schemaDefMap <$> MP.many schemaDefsP) <*> root) <* MP.eof

-------------------------------------------------------------------------------

parseSchemaAST :: FilePath -> Text -> Either Text Schema
parseSchemaAST fp t =
  case result of
    Left err -> Left (Text.pack $ MP.errorBundlePretty err)
    Right s  -> Right s
  where
    result = State.evalState (MP.runParserT schema fp t) emptyEnv

-- | Parse a 'Schema' from the given file, returning the AST itself
schemaASTFromFile :: FilePath -> IO (Either Text Schema)
schemaASTFromFile fp = parseSchemaAST fp <$> TIO.readFile fp

parseSchemaEnv :: FilePath -> Text -> Either Text Env
parseSchemaEnv fp t =
    case result of
      (Left err, _) -> Left (Text.pack $ MP.errorBundlePretty err)
      (Right _, s)  -> Right s
  where
    result = State.runState (MP.runParserT schema fp t) emptyEnv

-- | Parse a 'Schema' from the given file, returning the typing 'Env' it
-- specifies
schemaEnvFromFile :: FilePath -> IO (Either Text Env)
schemaEnvFromFile fp = parseSchemaEnv fp <$> TIO.readFile fp
