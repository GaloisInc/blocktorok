{-|
Module      : Language.Common.Units.Factor
Description : Representation of unit factors
Copyright   : (c) Galois, Inc. 2021
License     : N/A
Maintainer  : cphifer@galois.com
Stability   : experimental
Portability : N/A

The representation of the factors a unit is composed of. Useful for
mathematical manipulation.
-}

module Language.Common.Units.Factor
  ( ($=)
  , (@@+)
  , (@+)
  , (@@-)
  , (@-)
  , extract
  , normalize
  , reorder
  ) where

import Language.Common.Units.SI

-- | A unit factor (unit & exponent)
data Factor = F Unit Integer deriving (Eq)

infix 4 $=
-- | Return true iff the two given factors represent the same dimension
($=) :: Factor -> Factor -> Bool
(F u1 _) $= (F u2 _) = u1 == u2

-- | @extract s l@ pulls the 'Factor' matching @s@ out of @l@, returning the
-- rest of the list and (if it exists) the extracted factor
extract :: Factor -> [Factor] -> ([Factor], Maybe Factor)
extract _ []    = ([], Nothing)
extract s (h:t) =
  if s $= h
  then (t, Just h)
  else let (resList, resVal) = extract s t in (h : resList, resVal)

-- | @reorder fs1 fs2@ reorders the list of factors @fs1@ to match the order of
-- @fs2@, putting anything not in @fs2@ at the end
reorder :: [Factor] -> [Factor] -> [Factor]
reorder fs1 [] = fs1
reorder fs1 (h:t) =
  case extract h fs1 of
    (l, Nothing) -> reorder l t
    (l, Just f) -> f : reorder l t

-- | Remove all factors with exponent 0 from the given @[Factor]@
normalize :: [Factor] -> [Factor]
normalize []          = []
normalize (F _ 0 : t) = normalize t
normalize (h : t)     = h : normalize t

infixl 6 @@+
-- | Add corresponding exponents, assuming similar ordering
(@@+) :: [Factor] -> [Factor] -> [Factor]
[]             @@+ fs2                       = fs2
fs1            @@+ []                        = fs1
(F u1 e1 : t1) @@+ (F u2 e2 : t2) | u1 == u2 = F u1 (e1 + e2) : t1 @@+ t2
(h : t)        @@+ fs2                       = h : (t @@+ fs2)

infixl 6 @+
-- | Add corresponding exponents, preserving order
(@+) :: [Factor] -> [Factor] -> [Factor]
fs1 @+ fs2 = fs1 @@+ reorder fs2 fs1

infixl 6 @@-
-- | Subtract corresponding exponents, assuming similar ordering
(@@-) :: [Factor] -> [Factor] -> [Factor]
[]             @@- fs2                       = negDim <$> fs2
fs1            @@- []                        = fs1
(F u1 e1 : t1) @@- (F u2 e2 : t2) | u1 == u2 = F u1 (e1 - e2) : t1 @@- t2
(h : t)        @@- fs2                       = h : (t @@- fs2)

infixl 6 @-
-- | Subtract corresponing exponents, preserving order
(@-) :: [Factor] -> [Factor] -> [Factor]
fs1 @- fs2 | fs1 == fs2 = []
           | otherwise  = fs1 @@- reorder fs2 fs1

-- | Negate a single 'Factor'
negDim :: Factor -> Factor
negDim (F u e) = F u (negate e)
