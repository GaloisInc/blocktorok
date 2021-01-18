{-# LANGUAGE LambdaCase, NoMonomorphismRestriction, FlexibleContexts, RankNTypes,
             DeriveGeneric, DeriveDataTypeable, CPP, StandaloneDeriving #-}
{-# OPTIONS_HADDOCK prune #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parse.Units
-- Copyright   :  (C) 2014 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module defines a parser for unit expressions.  The syntax for
-- these expressions is like F#'s. There are four arithmetic operators
-- (@*@, @/@, @^@, and juxtaposition).  Exponentiation binds the
-- tightest, and it allows an integer to its right (possibly with
-- minus signs and parentheses). Next tightest is juxtaposition, which
-- indicates multiplication. Because juxtaposition binds tighter than
-- division, the expressions @m/s^2@ and @m/s s@ are
-- equivalent. Multiplication and division bind the loosest and are
-- left-associative, meaning that @m/s*s@ is equivalent to @(m/s)*s@,
-- probably not what you meant. Parentheses in unit expressions are
-- allowed, of course.
--
-- Within a unit string (that is, a unit with an optional prefix),
-- there may be ambiguity. If a unit string can be interpreted as a
-- unit without a prefix, that parsing is preferred. Thus, @min@ would
-- be minutes, not milli-inches (assuming appropriate prefixes and
-- units available.) There still may be ambiguity between unit
-- strings, even interpreting the string as a prefix and a base
-- unit. If a unit string is amiguous in this way, it is rejected.
-- For example, if we have prefixes @da@ and @d@ and units @m@ and
-- @am@, then @dam@ is ambiguous like this.
-----------------------------------------------------------------------------

module Text.Parse.Units (
  -- * Parsing units
  UnitExp(..), parseUnit,

  -- * Symbol tables
  SymbolTable(..), PrefixTable, UnitTable, mkSymbolTable,
  unsafeMkSymbolTable, universalSymbolTable,

  unitStringParser  -- these are pruned from the Haddock output
  ) where

import Prelude hiding ( lex, div )

import GHC.Generics (Generic)
import Text.Parsec         hiding ( tab )
import qualified Data.Map.Strict as Map
import qualified Data.MultiMap as MM
import Control.Monad.Reader
import Control.Arrow       hiding ( app)
import Data.Data (Data)
import Data.Maybe
import Data.Char

import Text.Lexer (Token(..))
import Text.Token
import Text.TokenClass

#if __GLASGOW_HASKELL__ < 709
import Data.Typeable ( Typeable )
#endif

----------------------------------------------------------------------
-- Basic combinators
----------------------------------------------------------------------

-- copied from GHC
partitionWith :: (a -> Either b c) -> [a] -> ([b], [c])
partitionWith _ [] = ([],[])
partitionWith f (x:xs) = case f x of
                         Left  b -> (b:bs, cs)
                         Right c -> (bs, c:cs)
    where (bs,cs) = partitionWith f xs

----------------------------------------------------------------------
-- Extra parser combinators
----------------------------------------------------------------------

-- | @experiment p@ runs @p@. If @p@ succeeds, @experiment@ returns the
-- result of running @p@. If @p@ fails, then @experiment@ returns @Nothing@.
-- In either case, no input is consumed and @experiment@ never fails.
experiment :: Stream s m t => ParsecT s u m a -> ParsecT s u m (Maybe a)
experiment = lookAhead . optionMaybe . try

----------------------------------------------------------------------
-- Datatypes
----------------------------------------------------------------------

-- | Parsed unit expressions, parameterized by a prefix identifier type and
-- a unit identifier type
data UnitExp pre u = Unity                     -- ^ "1"
                   | Unit (Maybe pre) u        -- ^ a unit with, perhaps, a prefix
                   | Mult (UnitExp pre u) (UnitExp pre u)
                   | Div (UnitExp pre u) (UnitExp pre u)
                   | Pow (UnitExp pre u) Integer
                   deriving (Eq, Ord, Generic, Data)

#if __GLASGOW_HASKELL__ < 709
deriving instance Typeable UnitExp
#endif

instance (Show pre, Show u) => Show (UnitExp pre u) where
  show Unity               = "1"
  show (Unit (Just pre) u) = show pre ++ " :@ " ++ show u
  show (Unit Nothing u)    = show u
  show (Mult e1 e2)        = "(" ++ show e1 ++ " :* " ++ show e2 ++ ")"
  show (Div e1 e2)         = "(" ++ show e1 ++ " :/ " ++ show e2 ++ ")"
  show (Pow e i)           = show e ++ " :^ " ++ show i

----------------------------------------------------------------------
-- Symbol tables
----------------------------------------------------------------------

-- | A finite mapping from prefix spellings to prefix identifiers (of
-- unspecified type @pre@). All prefix spellings must be strictly alphabetic.
type PrefixTable pre = Map.Map String pre

-- | A mapping from unit spellings to unit identifiers (of unspecified type
-- @u@). All unit spellings must be strictly alphabetic.
type UnitTable u = String -> Maybe u

-- | A "symbol table" for the parser, mapping prefixes and units to their
-- representations.
data SymbolTable pre u = SymbolTable { prefixTable :: PrefixTable pre
                                     , unitTable   :: UnitTable u
                                     } deriving (Generic)

-- | Build a 'Map' from an association list, checking for ambiguity
unambFromList :: (Ord a, Show b) => [(a,b)] -> Either [(a,[String])] (Map.Map a b)
unambFromList list =
  let multimap      = MM.fromList list
      assocs        = MM.assocs multimap
      (errs, goods) = partitionWith (\(key, vals) ->
                                       case vals of
                                         [val] -> Right (key, val)
                                         _     -> Left (key, map show vals)) assocs
      result        = Map.fromList goods
  in
  if null errs then Right result else Left errs

-- | Build a symbol table from prefix mappings and unit mappings. The prefix mapping
-- can be empty. This function checks to make sure that the strings are not
-- inherently ambiguous and are purely alphabetic.
mkSymbolTable :: (Show pre, Show u)
              => [(String, pre)]   -- ^ Association list of prefixes
              -> [(String, u)]     -- ^ Association list of units
              -> Either String (SymbolTable pre u)
mkSymbolTable prefixes units =
  let bad_strings = filter (not . all isLetter) (map fst prefixes ++ map fst units) in
  if not (null bad_strings)
  then Left $ "All prefixes and units must be composed entirely of letters.\nThe following are illegal: " ++ show bad_strings
  else
  let result = do
        prefixTab <- unambFromList prefixes
        unitTab   <- unambFromList units
        return $ SymbolTable { prefixTable = prefixTab, unitTable = flip Map.lookup unitTab }
  in left ((++ error_suffix) . concatMap mk_error_string) result
  where
    mk_error_string :: Show x => (String, [x]) -> String
    mk_error_string (k, vs) =
      "The label `" ++ k ++ "' is assigned to the following meanings:\n" ++
      show vs ++ "\n"
    error_suffix = "This is ambiguous. Please fix before building a unit parser."

-- | Make a symbol table without checking for ambiguity or non-purely
-- alphabetic strings.  The prefixes must be a (potentially empty)
-- finite map, but the units mapping need not be finite.
-- Note that this is unsafe in that the resulting parser may behave
-- unpredictably. It surely won't launch the rockets, though.
unsafeMkSymbolTable :: PrefixTable pre -> UnitTable u -> SymbolTable pre u
unsafeMkSymbolTable = SymbolTable

-- | A symbol table that accepts all unit strings, but supports no prefixes.
universalSymbolTable :: SymbolTable a String
universalSymbolTable = SymbolTable Map.empty Just


----------------------------------------------------------------------
-- Unit string parser
----------------------------------------------------------------------

-- We assume that no symbol table is inherently ambiguous!

type GenUnitStringParser pre u = ParsecT String () (Reader (SymbolTable pre u))
type UnitStringParser_UnitExp =
  forall pre u. (Show pre, Show u) => GenUnitStringParser pre u (UnitExp pre u)

-- parses just a unit (no prefix)
justUnitP :: GenUnitStringParser pre u u
justUnitP = do
  full_string <- getInput
  units <- asks unitTable
  case units full_string of
    Nothing -> fail (full_string ++ " does not match any known unit")
    Just u  -> return u

-- parses a unit and prefix, failing in the case of ambiguity
prefixUnitP :: UnitStringParser_UnitExp
prefixUnitP = do
  prefixTab <- asks prefixTable
  let assocs = Map.assocs prefixTab  -- these are in the right order
  results <- catMaybes `liftM` mapM (experiment . parse_one) assocs
  full_string <- getInput
  case results of
    [] -> fail $ "No known interpretation for " ++ full_string
    [(pre_name, unit_name)] ->
      return $ Unit (Just pre_name) unit_name
    lots -> fail $ "Multiple possible interpretations for " ++ full_string ++ ":\n" ++
                   (concatMap (\(pre_name, unit_name) ->
                                 "  " ++ show pre_name ++
                                 " :@ " ++ show unit_name ++ "\n") lots)
  where
    parse_one :: (String, pre) -> GenUnitStringParser pre u (pre, u)
    parse_one (pre, name) = do
      void $ string pre
      unit_name <- justUnitP
      return (name, unit_name)

-- parse a unit string
unitStringParser :: UnitStringParser_UnitExp
unitStringParser = try (Unit Nothing `liftM` justUnitP) <|> prefixUnitP

----------------------------------------------------------------------
-- Unit expression parser
----------------------------------------------------------------------

type GenUnitParser pre u = ParsecT [Token] () (Reader (SymbolTable pre u))
type UnitParser a = forall pre u. GenUnitParser pre u a
type UnitParser_UnitExp =
  forall pre u. (Show pre, Show u) => GenUnitParser pre u (UnitExp pre u)

-- parse a unit string
unitStringP :: String -> UnitParser_UnitExp
unitStringP str = do
  symbolTable <- ask
  case flip runReader symbolTable $ runParserT unitStringParser () "" str of
    Left err -> fail (show err)
    Right e  -> return e

-- parse a number, possibly negated and nested in parens
numP :: UnitParser Integer
numP =
  do tok' TokenLParen
     n <- numP
     tok' TokenRParen
     return n
  <|>
  do tok' TokenMinus
     negate <$> numP
  <|>
  number

-- parse an exponentiation, like "^2"
powP :: GenUnitParser pre u (UnitExp pre u -> UnitExp pre u)
powP = option id $ do
  tok' TokenPow
  flip Pow <$> numP

-- parse a unit, possibly with an exponent
unitP :: UnitParser_UnitExp
unitP =
  do n <- numP
     case n of
       1 -> return Unity
       _ -> unexpected $ "number " ++ show n
  <|>
  do unit_str <- unitT
     u <- unitStringP unit_str
     maybe_pow <- powP
     return $ maybe_pow u

-- parse a "unit factor": either a juxtaposed sequence of units
-- or a paranthesized unit exp.
unitFactorP :: UnitParser_UnitExp
unitFactorP =
  do tok' TokenLParen
     unitExp <- parseUnit
     tok' TokenRParen
     return unitExp
  <|>
  (foldl1 Mult <$> many1 unitP)

-- parse * or /
opP :: GenUnitParser pre u (UnitExp pre u -> UnitExp pre u -> UnitExp pre u)
opP = satisfy' p <?> "op"
  where p (Token _ t) = case t of
                          TokenTimes -> Just Mult
                          TokenDiv -> Just Div
                          _ -> Nothing

-- parse a whole unit expression
parseUnit :: UnitParser_UnitExp
parseUnit = chainl unitFactorP opP Unity

-- | Parse a unit expression, interpreted with respect the given symbol table.
-- Returns either an error message or the successfully-parsed unit expression.
-- parseUnit :: (Show pre, Show u)
--           => SymbolTable pre u -> String -> Either String (UnitExp pre u)
-- parseUnit tab s = left show $ do
--   toks <- lex s
--   flip runReader tab $ runParserT (consumeAll parser) () "" toks
