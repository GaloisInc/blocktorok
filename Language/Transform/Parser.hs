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

import           Control.Applicative            (many, some)
import           Control.Monad                  (void)
import           Control.Monad.Combinators.Expr (Operator (..), makeExprParser)

import qualified Data.List.NonEmpty             as NEL
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import qualified Data.Text.IO                   as TIO
import           Data.Void                      (Void)

import qualified Text.Megaparsec                as MP

import           Language.Common                (Located (..), location, unloc,
                                                 withSameLocAs)
import           Language.Common.Parser         (brackets, ident, lexeme,
                                                 lident, located, located', spc,
                                                 symbol')
import qualified Language.Common.Units.Parser   as UP
import           Language.Transform.Syntax      (Call (Call), Decl (..),
                                                 Expr (..),
                                                 FName (FFile, FHCat, FIsEmpty, FJoin, FMkSeq, FNot, FVCat, FVJoin),
                                                 Lit (LitString), Selector (..),
                                                 SelectorElement (..),
                                                 Transform (Transform))

type Parser a = MP.Parsec Void Text a

-------------------------------------------------------------------------------

selectorParser :: Parser Selector
selectorParser =
  do  e0 <- initial
      es <- MP.many subsequent
      pure $ Selector (e0 NEL.:| es)

  where
    schema = SelSchema <$> located (MP.try (symbol' "::") *> ident)
    mem = SelName <$> located (MP.try (symbol' ".") *> ident)
    cond = SelCond <$> (MP.try (symbol' "[") *> exprParser <* symbol' "]")
    initial =
      MP.choice [ SelName <$> lident, mem, schema ]
    subsequent =
      MP.choice [ mem, schema, cond ]

barStringExprParser :: Parser Expr
barStringExprParser =
  do  ls <- located $ some (lexeme line)
      case unloc ls of
        [a] -> pure a
        _   -> pure $ ExprFn (Call FVCat ls `withSameLocAs` ls)
  where
    line =
        do  void $ MP.chunk "|"
            elts <- located' (many stringElt)
            case unloc elts of
              [a] -> pure a
              _   -> pure $ ExprFn (Call FHCat elts `withSameLocAs` elts)
    stringElt = MP.choice [stringChunk, embeddedExpr, escaped]

    sc =
      MP.takeWhile1P Nothing (not . (`elem` ['\n', '\r', '$', '\\']))

    stringChunk = ExprLit . LitString <$> located' sc
    embeddedExpr =
      do  void $ symbol' "${"
          expr <- exprParser
          void $ MP.chunk "}"
          pure expr

    escaped =
      do  void $ MP.chunk "\\"
          c <- located' MP.anySingle
          pure . ExprLit . LitString $ (Text.singleton <$> c)

strLitParser :: Parser Text
strLitParser =
  do  symbol' "\""
      contents <- MP.takeWhileP (Just "string literal") (/= '"')
      void $ MP.single '"'
      pure contents

exprParser :: Parser Expr
exprParser = baseExpr
  where
    term = MP.choice [ mkSeq
                     , fn "vjoin" FVJoin
                     , fn "join" FJoin
                     , fn "file" FFile
                     , ExprLit . LitString <$> located strLitParser
                     , convert
                     , selector
                     , symbol' "(" *> exprParser <* symbol' ")"
                     ]

    binary name f = InfixL (f <$ symbol' name)
    prefix name f = Prefix (f <$ symbol' name)
    postfix name f = Postfix (f <$ symbol' name)

    opTable =
      [ [ postfix "!?" isEmpty
        , postfix "?" notIsEmpty
        ]
      , [ prefix "not" ExprNot ]
      , [ binary "and" ExprAnd
        , binary "or" ExprOr
        ]
      ]

    baseExpr =
      MP.choice [ forParser
                , cond
                , barStringExprParser
                , makeExprParser term opTable
                ]

    convert =
      do  symbol' "convert"
          symbol' "["
          unit <- located UP.parseUnit
          symbol' "]"
          symbol' "("
          expr <- exprParser
          symbol' ")"

          pure $ ExprConvertUnits expr unit

    cond =
      do  lite <- located $ do  symbol' "if"
                                branches <- MP.sepBy1 ((,) <$> exprParser <*> brackets exprParser) (symbol' "else if")
                                symbol' "else"
                                e <- brackets exprParser
                                pure (branches, e)
          let (branches, e) = unloc lite
          pure $ ExprCond (locRange lite) branches e

    isEmpty arg =
        let sr = location arg
            cl = Call FIsEmpty ([arg] `withSameLocAs` arg) `withSameLocAs` sr
        in ExprFn cl

    notIsEmpty arg =
        let sr = location arg
            ncall = Call FNot ([eexpr] `withSameLocAs` sr) `withSameLocAs` sr
            eexpr = ExprFn ecall
            ecall = Call FIsEmpty ([arg] `withSameLocAs` arg) `withSameLocAs` sr
        in ExprFn ncall

    selector = ExprSelector <$> selectorParser

    parseArgs =
      located $ MP.sepBy exprParser (symbol' ",")

    forParser =
      do  symbol' "for"
          name <- lident
          symbol' "in"
          ExprFor name <$> exprParser <*> exprParser

    call name fname =
      located $
      do  MP.try (symbol' name <* symbol' "(")
          Call fname <$> (parseArgs <* symbol' ")")

    fn name fname = ExprFn <$> call name fname

    mkSeq =
      do  seqb <- seqBody
          pure $ ExprFn (Call FMkSeq seqb `withSameLocAs` seqb)

    seqBody =
      located $ symbol' "[" *> MP.sepBy exprParser (symbol' ",") <* symbol' "]"


declParser :: Parser Decl
declParser = MP.choice [renderDecl, letDecl, outDecl, inDecl, requireDecl]
  where
    inDecl =
      do  lin <-
            located $
              do  sel <- symbol' "in" *> selectorParser <* symbol' "{"
                  decls <- many declParser
                  symbol' "}"
                  pure (sel, decls)
          let (sel, decls) = locValue lin

          pure $ DeclIn (locRange lin) sel decls

    outDecl =
      do  i <- MP.try $ lident <* symbol' "<<"
          DeclFileOut i <$> exprParser

    letDecl =
      do  i <- MP.try $ lident <* symbol' "="
          DeclLet i <$> exprParser

    renderDecl =
      do  symbol' "render"
          sel <- selectorParser
          DeclRender sel <$> exprParser

    requireDecl =
      do  symbol' "require"
          DeclRequire <$> exprParser <*> located strLitParser


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

