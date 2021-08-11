{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Language.Common.Units.Parser
Description : Parsing of SI units
Copyright   : (c) Galois, Inc. 2021
License     : N/A
Maintainer  : cphifer@galois.com
Stability   : experimental
Portability : N/A

Parsers for units to be used in the schema/transformer languages.
-}

module Language.Common.Units.Parser where

import Control.Monad.Reader (ReaderT, ask, asks, liftM, runReaderT, void)

import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)

import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MPC
import qualified Text.Megaparsec.Char.Lexer as Lexer

import Language.Common.Located (Located(..), SourceRange(..))
import Language.Common.Units.Combinators
import Language.Common.Units.SymbolTable
import Language.Common.Units.Units

type Parser m a = MP.ParsecT Void Text (ReaderT SymbolTable m) a

spc :: Monad m => Parser m ()
spc = Lexer.space MPC.space1
                  MP.empty
                  MP.empty

lexeme :: Monad m => Parser m a -> Parser m a
lexeme = Lexer.lexeme spc

symbol :: Monad m => Text -> Parser m Text
symbol = Lexer.symbol spc

symbol' :: Monad m => Text -> Parser m ()
symbol' t = void $ symbol t

mkRange :: MP.SourcePos -> MP.SourcePos -> SourceRange
mkRange s e =
  SourceRange (MP.sourceName s) (asLoc s) (asLoc e)
  where
    asLoc pos = (MP.unPos (MP.sourceLine pos), MP.unPos (MP.sourceColumn pos))

located :: Monad m => Parser m a -> Parser m (Located a)
located p =
  lexeme $
    do start <- MP.getSourcePos
       a <- p
       end <- MP.getSourcePos
       pure $ Located (mkRange start end) a

optional :: Monad m => Parser m a -> Parser m (Maybe a)
optional p = (Just <$> MP.try p) <|> pure Nothing

experiment :: Monad m => Parser m a -> Parser m (Maybe a)
experiment = MP.lookAhead . MP.optional . MP.try

chainl1 :: Monad m => Parser m a -> Parser m (a -> a -> a) -> Parser m a
chainl1 p op =
  do x <- p
     rest x
  where
    rest x =
      do f <- op
         y <- p
         rest (f x y)
      <|>
      return x

chainl :: Monad m => Parser m a -> Parser m (a -> a -> a) -> a -> Parser m a
chainl p op x = chainl1 p op <|> return x

justUnitP :: Monad m => Parser m Unit
justUnitP =
  do fullString <- Text.unpack <$> MP.getInput
     units <- asks unitTable
     case units fullString of
       Nothing -> fail (fullString ++ " does not match any known unit")
       Just u -> return u

prefixUnitP :: Monad m => Parser m Unit
prefixUnitP =
  do prefixTab <- asks prefixTable
     let assocs = Map.assocs prefixTab
     results <- catMaybes `liftM` mapM (experiment . parseOne) assocs
     fullString <- Text.unpack <$> MP.getInput
     case results of
       [] -> fail $ "No known interpretation for " ++ fullString
       [(pre, u)] ->
         return $ pre ||@ u
       _ -> fail $ "Multiple possible interpretations for " ++ fullString
  where
    parseOne :: Monad m => (String, Rational) -> Parser m (Rational, Unit)
    parseOne (preName, pre) =
      do void $ MPC.string $ Text.pack preName
         uName <- justUnitP
         return (pre, uName)

unitStringParser :: Monad m => Parser m Unit
unitStringParser = MP.try (justUnitP) <|> prefixUnitP

unitStringP :: Monad m => Text -> Parser m Unit
unitStringP s =
  do symbolTable <- ask
     case flip runReaderT symbolTable $ MP.runParserT unitStringParser "" s of
       Left err -> err
       Right e -> case e of
                    Left e' -> fail $ MP.errorBundlePretty e'
                    Right u -> return u

numP :: Monad m => Parser m Integer
numP =
  do symbol' "("
     n <- numP
     symbol' ")"
     return n
  <|>
  do symbol' "-"
     negate <$> numP
  <|>
  Lexer.signed (MP.empty) (lexeme Lexer.decimal)

powP :: Monad m => Parser m (Unit -> Unit)
powP = MP.option id $
  do symbol' "^"
     flip (||^) <$> numP

unitP :: Monad m => Parser m Unit
unitP =
  do n <- numP
     case n of
       1 -> return number
       _ -> fail $ "Unexpected number: " ++ show n
  <|>
  do unitStr <- MP.some MPC.letterChar
     u <- unitStringP $ Text.pack unitStr
     mPow <- powP
     return $ mPow u

unitFactorP :: Monad m => Parser m Unit
unitFactorP =
  do symbol' "("
     u <- parseUnit
     symbol' ")"
     return u
  <|>
  (foldl1 (||*) <$> MP.some unitP)

opP :: Monad m => Parser m (Unit -> Unit -> Unit)
opP =
  do op <- MP.choice [symbol "*", symbol "/"]
     case op of
       "*" -> return (||*)
       "/" -> return (||/)
       _ -> fail "Unrecognized operator"

parseUnit :: Monad m => Parser m Unit
parseUnit = chainl unitFactorP opP number
