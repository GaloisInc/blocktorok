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

import           Control.Applicative          ((<**>))
import           Control.Monad.State          (State)
import qualified Control.Monad.State          as State

import           Data.Functor                 (($>))
import qualified Data.MultiSet                as MS
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Text.IO                 as TIO
import           Data.Void                    (Void)

import           Text.Megaparsec              ((<|>))
import qualified Text.Megaparsec              as MP
import qualified Text.Megaparsec.Char         as MPC
import qualified Text.Megaparsec.Char.Lexer   as Lexer

import           Language.Common              (unloc)
import           Language.Common.Parser       (brackets, ident, keyword, lident,
                                               located, optional, spc, symbol')
import qualified Language.Common.Units.Parser as UP
import           Language.Schema.Env          (Env, addRootType, addTypeDef,
                                               emptyEnv, lookupTypeDef)
import           Language.Schema.Syntax       (BlockDecl (BlockDecl, blockDeclDecl),
                                               BlockS (BlockS),
                                               Decl (Decl, declName),
                                               Root (Root), Schema (Schema),
                                               SchemaDef (..), Union (Union),
                                               Variant (Variant, variantTag),
                                               globbedDeclsMap, schemaDefMap,
                                               variantsMap)
import           Language.Schema.Type         (Globbed (..), Ident, SType (..),
                                               unGlob)

type Parser a = MP.ParsecT Void Text (State Env) a

selector :: Parser Ident
selector = MPC.char '.' *> ident

stype :: Parser SType
stype =
  MP.choice [ keyword "int" $> SInt
            , keyword "bool" $> SBool
            , keyword "float" $> SFloat
            , keyword "string" $> SString
            , parseQuantity
            , SNamed <$> ident
            ]
  where
    parseQuantity =
      do  keyword "quantity"
          SQuantity <$> (symbol' "(" *> UP.parseUnit <* symbol' ")")

-- ! Uses MP.setOffset; parsing state is messed up after failure
decl :: Parser Ident -> Parser Decl
decl p =
  do n <- located p
     symbol' ":"
     o <- MP.getOffset
     t <- located stype
     let res = Decl n t
     case unloc t of
       SNamed nm ->
         do env <- State.get
            case lookupTypeDef nm env of
              Nothing -> MP.setOffset o >> fail ("The type " ++ show nm ++ " is not defined")
              Just _ -> pure res
       _ -> pure res

doc :: Parser Text
doc = Text.pack <$> (symbol' "[-- " *> MP.manyTill Lexer.charLiteral (symbol' " --]"))

variant :: Parser Variant
variant =
  Variant <$> optional (located doc)
          <*> lident
          <*> optional stype
          <*  symbol' ";"

duplicatedElems :: Ord a => [a] -> [a]
duplicatedElems xs = [x | (x, c) <- MS.toOccurList (MS.fromList xs), c > 1]

-- ! Uses MP.setOffset; parsing state is messed up after failure
union :: Parser Union
union =
  do symbol' "union"
     o    <- MP.getOffset
     nm   <- lident
     vars <- brackets $ MP.some variant
     let dupedTags   = duplicatedElems (fmap (unloc . variantTag) vars)
         ppDupedTags = Text.unpack $ Text.intercalate ", " dupedTags
     if null dupedTags then
       do let u = Union nm (variantsMap vars)
          addTypeDef (unloc nm) (UnionDef u)
          pure u
     else
       MP.setOffset o >> fail ("This union has duplicate tags: " ++ ppDupedTags)

glob :: Parser (a -> Globbed a)
glob = MP.option One $ MP.choice [opt, some, many]
  where
    opt  = symbol' "?" $> Optional
    some = symbol' "+" $> Some
    many = symbol' "*" $> Many

blockDecl :: Parser BlockDecl
blockDecl =
  BlockDecl <$> optional (located doc)
            <*> decl selector

-- ! Uses MP.setOffset; parsing state is messed up after failure
blockS :: Parser BlockS
blockS =
  do symbol' "block"
     o <- MP.getOffset
     t  <- lident
     fs <- brackets $ MP.many $ blockDecl <**> glob
     let dupedFields   = duplicatedElems (fmap (unloc . declName . blockDeclDecl . unGlob) fs)
         ppDupedFields = Text.unpack $ Text.intercalate ", " dupedFields
     if null dupedFields then
       do let b = BlockS t (globbedDeclsMap fs)
          addTypeDef (unloc t) (BlockDef b)
          pure b
     else
       MP.setOffset o >> fail ("This block has duplicated fields: " ++ ppDupedFields)

-- ! Uses MP.setOffset; parsing state is messed up after failure
root :: Parser Root
root =
  do o <- MP.getOffset
     symbol' "root"
     fs <- brackets $ MP.many $ blockDecl <**> glob
     let dupedFields   = duplicatedElems (fmap (unloc . declName . blockDeclDecl . unGlob) fs)
         ppDupedFields = Text.unpack $ Text.intercalate ", " dupedFields
     if null dupedFields then
       do sequence_ (addRootType <$> fs)
          pure $ Root $ globbedDeclsMap fs
     else
       MP.setOffset o >> fail ("The root specification has duplicate fields: " ++ ppDupedFields)

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
