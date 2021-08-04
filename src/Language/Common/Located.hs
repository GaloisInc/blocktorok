{-|
Module      : Language.Common.Located
Description : Syntactic location information for languages
Copyright   : (c) Galois, Inc. 2021
License     : N/A
Maintainer  : cphifer@galois.com
Stability   : experimental
Portability : N/A

Types and associated functions/instances for storing source location
data for error reporting.
-}

module Language.Common.Located
  ( SourceRange(..)
  , Located(..)
  , withSameLocAs
  , ppRange
  ) where

import Data.Text(Text, pack)

-- | A line:column range in a source file used for error reporting
data SourceRange = SourceRange
  { sourceRangeFile :: FilePath
  , sourceRangeStart :: (Int, Int)
  , sourceRangeEnd :: (Int, Int)
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
