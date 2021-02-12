{-# LANGUAGE DeriveGeneric #-}

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

module Data.Units.SymbolTable
  ( SymbolTable(..)
  , PrefixTable
  , UnitTable
  , mkSymbolTable
  , unsafeMkSymbolTable
  , universalSymbolTable
  ) where

import GHC.Generics (Generic)
import qualified Data.Map.Strict as Map
import qualified Data.MultiMap as MM
import Control.Arrow (left)
import Data.Char (isLetter)

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