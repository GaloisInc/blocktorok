{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Units.UnitExp
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

module Data.Units.UnitExp
  ( UnitExp(..)
  ) where

import GHC.Generics (Generic)
import Data.Data (Data)

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
