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
  ( -- * Source Locations
    -- ** Types
    SourceRange(..)
  , Located(..)
    -- ** Classes
  , HasLocation(..)
    -- ** Computing with locations
  , locUnknown
  , msgWithLoc
  , orThrow
  , orThrow'
  , sourceRangeSpan
  , sourceRangeSpan'
  , unloc
  , withSameLocAs
    -- ** Pretty-printing
  , ppRange
  ) where

import qualified Control.Exception         as Ex

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
withSameLocAs :: HasLocation a => b -> a -> Located b
withSameLocAs b a = Located (location a) b

-- | Pretty-printing for a 'SourceRange'
ppRange :: SourceRange -> Text
ppRange (SourceRange path (startLn, startCol) (endLn, endCol)) =
  pack $
    path ++ ":" ++ show startLn ++ ":" ++ show startCol ++ "-"
                ++ show endLn ++ ":" ++ show endCol

instance Functor Located where
  fmap f a = a { locValue = f (locValue a) }

-- | @locUnknown a@ returns @a@ annoated with a stub 'SourceRange' indicating
-- that the location is not known
locUnknown :: a -> Located a
locUnknown = Located (SourceRange "unknown!" (0, 0) (0, 0))

-- | A class for types carrying source location information
class HasLocation a where
  -- | @location a@ returns the 'SourceRange' carried by @a@
  location :: a -> SourceRange

instance HasLocation (Located a) where
  location = locRange

instance HasLocation SourceRange where
  location = id

-- | @msgWithLoc why msg@ prepends the location information carried by @why@
-- to @msg@
msgWithLoc :: HasLocation a => a -> Text -> Text
msgWithLoc why msg = ppRange (location why) <> ": " <> msg

-- | Return the underlying value of something that is 'Located'
unloc :: Located a -> a
unloc = locValue

-- | @sourceRangeSpan sr1 sr2@ computes a new 'SourceRange' which is the total
-- range spanned by @sr1@ and @sr2@
sourceRangeSpan :: SourceRange -> SourceRange -> SourceRange
sourceRangeSpan (SourceRange f1 s1 e1) (SourceRange _ s2 e2) =
    SourceRange f1 (fst $ sortRanges s1 s2) (snd $ sortRanges e1 e2)
  where
    sortRanges sr1@(r1, c1) sr2@(r2, c2) =
      if r1 < r2 || (r1 == r2 && c1 < c2)
        then (sr1, sr2)
        else (sr2, sr1)

-- | Polymorphic implementation of @sourceRangeSpan@ for types carrying
-- location data
sourceRangeSpan' :: (HasLocation a, HasLocation b) => a -> b -> SourceRange
sourceRangeSpan' e1 e2 = sourceRangeSpan (location e1) (location e2)

-- | @ioe `orThrow` f@ runs the 'IO' action @ioe@, throwing an exception via
-- @f@ if the result is @Left@ and unwrapping the @Right@ value otherwise
orThrow :: Ex.Exception c => IO (Either a b) -> (a -> c) -> IO b
orThrow io mkC  =
  do  eitherB <- io
      case eitherB of
        Left a  -> Ex.throwIO (mkC a)
        Right b -> pure b

-- | The same as 'orThrow', except taking a pure 'Either' value rather than an
-- 'IO' action
orThrow' :: Ex.Exception c => Either a b -> (a -> c) -> IO b
orThrow' e = orThrow (pure e)
