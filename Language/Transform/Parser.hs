{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Language.Transform.Parser
Description : Parsers for the transformer language
Copyright   : (c) Galois, Inc. 2021
License     : N/A
Maintainer  : james.lamar@galois.com
Stability   : experimental
Portability : N/A

Parsing of Blocktorok transformers, exposing only an interface to parse
transformers from files in 'IO' contexts.
-}

module Language.Transform.Parser
  ( -- * Parsing Blocktorok transformers
    transformFromFile
  ) where

import           Control.Applicative        (many, some)
import           Control.Monad              (void)

import           Data.Char                  (isAlpha, isDigit)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.IO               as TIO
import           Data.Void                  (Void)

import qualified Text.Megaparsec            as MP
import qualified Text.Megaparsec.Char       as MPC
import qualified Text.Megaparsec.Char.Lexer as Lexer

import           Language.Common            (Located (..),
                                             SourceRange (SourceRange), unloc,
                                             withSameLocAs)
import           Language.Transform.Syntax  (Call (Call), Decl (..), Expr (..),
                                             FName (FFile, FHCat, FJoin, FMkSeq, FVCat),
                                             Lit (LitString), Selector (..),
                                             Transform (Transform))

type Parser a = MP.Parsec Void Text a

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
located = lexeme . located'

located' :: Parser a -> Parser (Located a)
located' p =
    do  start <- MP.getSourcePos
        a <- p
        end <- MP.getSourcePos
        pure $ Located (mkRange start end) a

ident :: Parser Text
ident =
  lexeme . MP.try $
    do  c0 <- MP.satisfy identFirstChar
        cr <- MP.takeWhileP Nothing identRestChar
        pure (c0 `Text.cons` cr)
  where
    identFirstChar :: Char -> Bool
    identFirstChar c = isAlpha c || c == '_'
    identRestChar c = identFirstChar c || isDigit c

lident :: Parser (Located Text)
lident = located ident

-------------------------------------------------------------------------------

selectorParser :: Parser Selector
selectorParser =
  do  nm <- lident
      selElt (SelName nm)
  where
    selElt p =
      MP.choice [selMem p, pure p]
    selMem p =
      do  p' <- MP.try (symbol' ".") *> (SelMem p <$> lident)
          selElt p'

barStringExprParser :: Parser Expr
barStringExprParser =
  do  ls <- located $ some (lexeme line)
      case unloc ls of
        [a] -> pure a
        _   -> pure $ ExprFn (Call FVCat ls `withSameLocAs` ls)
  where
    line =
        do  MP.try (symbol' "|")
            elts <- located' (many stringElt)
            case unloc elts of
              [a] -> pure a
              _   -> pure $ ExprFn (Call FHCat elts `withSameLocAs` elts)
    stringElt = MP.choice [stringChunk, embeddedExpr, escaped]

    sc =
      MP.takeWhile1P Nothing (not . (`elem` ['\n', '\r', '$', '\\']))

    stringChunk = ExprLit . LitString <$> located' sc
    embeddedExpr =
      do  void $ MP.try (MP.chunk "${")
          expr <- exprParser
          void $ MP.chunk "}"
          pure expr

    escaped =
      do  void $ MP.try (MP.chunk "\\")
          c <- located' MP.anySingle
          pure . ExprLit . LitString $ (Text.singleton <$> c)

strLitParser :: Parser Text
strLitParser =
  do  MP.try (symbol' "\"")
      contents <- MP.takeWhileP (Just "string literal") (/= '"')
      void $ MP.single '"'
      pure contents

exprParser :: Parser Expr
exprParser =
    MP.choice [ mkSeq
              , fn "join" FJoin
              , fn "file" FFile
              , ExprLit . LitString <$> located strLitParser
              , barStringExprParser
              , selector
              ]
  where
    selector = ExprSelector <$> selectorParser

    parseArgs =
      located $
        symbol' "(" *>
          MP.sepBy exprParser (symbol' ",")
        <* symbol' ")"


    call name fname =
      located $
      do  MP.try (symbol' name)
          Call fname <$> parseArgs

    fn name fname = ExprFn <$> call name fname

    mkSeq =
      do  seqb <- seqBody
          pure $ ExprFn (Call FMkSeq seqb `withSameLocAs` seqb)

    seqBody =
      located $
        MP.try (symbol' "[") *>
          MP.sepBy exprParser (symbol' ",")
        <* MP.try (symbol' "]")


declParser :: Parser Decl
declParser = MP.choice [renderDecl, letDecl, outDecl]
  where
    outDecl =
      do  i <- MP.try $ lident <* symbol' "<<"
          DeclFileOut i <$> exprParser

    letDecl =
      do  i <- MP.try $ lident <* symbol' "="
          DeclLet i <$> exprParser

    renderDecl =
      do  MP.try (symbol' "render")
          sel <- selectorParser
          DeclRender sel <$> exprParser

transformParser :: Parser Transform
transformParser =
  do  spc
      schema <- symbol' "schema" *> located strLitParser
      decls <- many declParser
      pure $ Transform schema decls

-------------------------------------------------------------------------------

parseTransform :: String -> Text -> Either Text Transform
parseTransform name input =
  case MP.parse (transformParser <* MP.eof) name input of
    Right tx -> Right tx
    Left err -> Left (Text.pack $ MP.errorBundlePretty err)

-- | Parse a 'Transform' from the contents of a given file
transformFromFile :: FilePath -> IO (Either Text Transform)
transformFromFile fp =
  parseTransform fp <$> TIO.readFile fp

