{-|
Module      : Language.Common.Units.SymbolTable
Description : Symbol table for unit parsing
Copyright   : (c) Galois, Inc. 2021
License     : N/A
Maintainer  : cphifer@galois.com
Stability   : experimental
Portability : N/A

Definitions related to a "symbol table" for unit parsing.
-}

module Language.Common.Units.SymbolTable where

import           Control.Arrow               (left)

import           Data.Char                   (isLetter)
import qualified Data.Map.Strict             as Map
import qualified Data.MultiMap               as MM

import           Language.Common.Units.Units (Unit)

partitionWith :: (a -> Either b c) -> [a] -> ([b], [c])
partitionWith _ [] = ([], [])
partitionWith f (x:xs) = case f x of
                           Left  b -> (b:bs, cs)
                           Right c -> (bs, c:cs)
  where (bs, cs) = partitionWith f xs

type PrefixTable = Map.Map String Rational
type UnitTable = String -> Maybe Unit

data SymbolTable = SymbolTable
  { prefixTable :: PrefixTable
  , unitTable   :: UnitTable
  }

unambFromList :: (Ord a, Show b) => [(a, b)] -> Either [(a, [String])] (Map.Map a b)
unambFromList list =
  let multimap      = MM.fromList list
      assocs        = MM.assocs multimap
      (errs, goods) = partitionWith (\(key, vals) ->
                                       case vals of
                                         [val] -> Right (key, val)
                                         _     -> Left (key, map show vals)) assocs
      result = Map.fromList goods
  in
    if null errs then Right result else Left errs

mkSymbolTable :: [(String, Rational)]
              -> [(String, Unit)]
              -> Either String SymbolTable
mkSymbolTable prefixes units =
  let bad_strings = filter (not . all isLetter) (map fst prefixes ++ map fst units) in
  if not (null bad_strings)
  then Left $ "All prefixes and units must be composed entirely of letters.\nThe following are illegal: " ++ show bad_strings
  else
  let result = do prefixTab <- unambFromList prefixes
                  unitTab <- unambFromList units
                  return $ SymbolTable { prefixTable = prefixTab, unitTable = flip Map.lookup unitTab }
  in left ((++ errorSuffix) . concatMap mkErrString) result
  where
    mkErrString :: Show x => (String, [x]) -> String
    mkErrString (k, vs) =
      "The label `" ++ k ++ "` is assigned to the following meanings:\n" ++
      show vs ++ "\n"
    errorSuffix = "This is ambiguous. Please fix before building a unit parser."
