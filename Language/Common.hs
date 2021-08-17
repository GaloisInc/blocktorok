{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Language.Common
Description : Common types / functions for languages
Copyright   : (c) Galois, Inc. 2021
License     : N/A
Maintainer  : cphifer@galois.com
Stability   : experimental
Portability : N/A

Common types and functions used for various language syntaxes, parsers, etc.
-}

module Language.Common
  ( SourceRange(..)
  , Located(..)
  , HasLocation(..)
  , withSameLocAs
  , ppRange
  , locUnknown
  , msgWithLoc
  , unloc
  , sourceRangeSpan
  , sourceRangeSpan'
  ) where

import           Data.Text (Text, pack)

-- | A line:column range in a source file used for error reporting
data SourceRange = SourceRange
  { sourceRangeFile  :: FilePath
  , sourceRangeStart :: (Int, Int)
  , sourceRangeEnd   :: (Int, Int)
  }
  deriving(Show, Eq, Ord)

-- | A value located within a particular 'SourceRange'
data Located a = Located
  { locRange :: SourceRange
  , locValue :: a
  }
  deriving(Show, Eq, Ord)


-- | @withSameLocAs b a@ returns @b@ annotated with the same location
-- information as @a@
withSameLocAs :: b -> Located a -> Located b
withSameLocAs b a = Located (locRange a) b

-- | Pretty-printing for a 'SourceRange'
ppRange :: SourceRange -> Text
ppRange (SourceRange path (startLn, startCol) (endLn, endCol)) =
  pack $
    path ++ ":" ++ show startLn ++ ":" ++ show startCol ++ "-"
                ++ show endLn ++ ":" ++ show endCol

instance Functor Located where
  fmap f a = a { locValue = f (locValue a) }

locUnknown :: a -> Located a
locUnknown = Located (SourceRange "unknown!" (0, 0) (0, 0))

class HasLocation a where
  location :: a -> SourceRange

instance HasLocation (Located a) where
  location = locRange

instance HasLocation SourceRange where
  location = id

msgWithLoc :: HasLocation a => a -> Text -> Text
msgWithLoc why msg = ppRange (location why) <> ": " <> msg

unloc :: Located a -> a
unloc = locValue

sourceRangeSpan :: SourceRange -> SourceRange -> SourceRange
sourceRangeSpan (SourceRange f1 s1 e1) (SourceRange _ s2 e2) =
    SourceRange f1 (fst $ sortRanges s1 s2) (snd $ sortRanges e1 e2)
  where
    sortRanges sr1@(r1, c1) sr2@(r2, c2) =
      if r1 < r2 || (r1 == r2 && c1 < c2)
        then (sr1, sr2)
        else (sr2, sr1)

sourceRangeSpan' :: (HasLocation a, HasLocation b) => a -> b -> SourceRange
sourceRangeSpan' e1 e2 = sourceRangeSpan (location e1) (location e2)
