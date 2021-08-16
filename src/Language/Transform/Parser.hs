{-# Language OverloadedStrings #-}
module Language.Transform.Parser where

import Data.Char ( isDigit, isAlpha )
import Data.Text(Text, pack)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import Data.Void(Void)
import Control.Monad(void)
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MPC
import Control.Applicative(many, some, (<|>))
import Language.Common
    ( SourceRange(SourceRange), Located(..), unloc, withSameLocAs)
import Language.Transform.Syntax



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

ppRange :: SourceRange -> Text
ppRange (SourceRange path (startLn, startCol) (endLn, endCol)) =
  pack $
    path ++ ":" ++ show startLn ++ ":" ++ show startCol ++ "-"
                ++ show endLn ++ ":" ++ show endCol

located :: Parser a -> Parser (Located a)
located = lexeme . located'

located' :: Parser a -> Parser (Located a)
located' p =
    do  start <- MP.getSourcePos
        a <- p
        end <- MP.getSourcePos
        pure $ Located (mkRange start end) a

withLoc' :: Parser (SourceRange -> a) -> Parser a
withLoc' p =
  do  start <- MP.getSourcePos
      a <- p
      end <- MP.getSourcePos
      pure $ a (mkRange start end)

withLoc :: Parser (SourceRange -> a) -> Parser a
withLoc = lexeme . withLoc'

optional :: Parser a -> Parser (Maybe a)
optional p = (Just <$> MP.try p) <|> pure Nothing

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
  do  ls <- located $ some line
      case unloc ls of
        [a] -> pure a
        _ -> pure $ ExprFn (Call FVCat ls `withSameLocAs` ls)
  where
    line =
        do  MP.try (symbol' "|")
            elts <- located' (many stringElt)
            pure $ ExprFn (Call FHCat elts `withSameLocAs` elts)
    stringElt = MP.choice [stringChunk, embeddedExpr, escaped]

    sc =
      MP.takeWhile1P Nothing (not . (`elem` ['\n', '\r', '$', '\\']))

    stringChunk = ExprLit . LitString <$> located' sc
    embeddedExpr =
      do  _ <- MP.try (MP.chunk "${")
          expr <- exprParser
          symbol' "}"

          pure expr
    escaped =
      do  _ <- MP.try (MP.chunk "\\")
          c <- located MP.anySingle
          pure . ExprLit . LitString $ (Text.singleton <$> c)

strLitParser :: Parser Text
strLitParser =
  do  MP.try (symbol' "\"")
      contents <- MP.takeWhileP (Just "string literal") (/= '"')
      _ <- MP.single '"'
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
          args <- parseArgs
          pure $ Call fname args

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
          expr <- exprParser
          pure $ DeclFileOut i expr

    letDecl =
      do  i <- MP.try $ lident <* symbol' "="
          expr <- exprParser
          pure $ DeclLet i expr

    renderDecl =
      do  MP.try (symbol' "render")
          sel <- selectorParser
          --symbol' "as"
          expr <- exprParser
          pure $ DeclRender sel expr

transformParser :: Parser Transform
transformParser =
  do  schema <- symbol' "schema" *> located strLitParser
      decls <- many declParser
      pure $ Transform schema decls

-------------------------------------------------------------------------------

parseTransform :: String -> Text -> Either Text Transform
parseTransform name input =
  case MP.parse (transformParser <* MP.eof) name input of
    Right tx -> Right tx
    Left err -> Left (Text.pack $ MP.errorBundlePretty err)

transformFromFile :: FilePath -> IO (Either Text Transform)
transformFromFile fp =
  parseTransform fp <$> TIO.readFile fp