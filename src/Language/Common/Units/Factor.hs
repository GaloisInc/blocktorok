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

module Language.Common.Units.Factor where

-- | A unit factor (unit & exponent)
data Factor a = F a Integer deriving (Eq)

infix 4 $=
-- | Return true iff the two given factors represent the same dimension
($=) :: (Eq a) => Factor a -> Factor a -> Bool
(F u1 _) $= (F u2 _) = u1 == u2

-- | @extract s l@ pulls the 'Factor' matching @s@ out of @l@, returning the
-- rest of the list and (if it exists) the extracted factor
extract :: Eq a => Factor a -> [Factor a] -> ([Factor a], Maybe (Factor a))
extract _ []    = ([], Nothing)
extract s (h:t) =
  if s $= h
  then (t, Just h)
  else let (resList, resVal) = extract s t in (h : resList, resVal)

-- | @reorder fs1 fs2@ reorders the list of factors @fs1@ to match the order of
-- @fs2@, putting anything not in @fs2@ at the end
reorder :: Eq a => [Factor a] -> [Factor a] -> [Factor a]
reorder fs1 [] = fs1
reorder fs1 (h:t) =
  case extract h fs1 of
    (l, Nothing) -> reorder l t
    (l, Just f) -> f : reorder l t

-- | Remove all factors with exponent 0 from the given @[Factor]@
normalize :: [Factor a] -> [Factor a]
normalize []          = []
normalize (F _ 0 : t) = normalize t
normalize (h : t)     = h : normalize t

infixl 6 @@+
-- | Add corresponding exponents, assuming similar ordering
(@@+) :: Eq a => [Factor a] -> [Factor a] -> [Factor a]
[]             @@+ fs2                       = fs2
fs1            @@+ []                        = fs1
(F u1 e1 : t1) @@+ (F u2 e2 : t2) | u1 == u2 = F u1 (e1 + e2) : t1 @@+ t2
(h : t)        @@+ fs2                       = h : (t @@+ fs2)

infixl 6 @+
-- | Add corresponding exponents, preserving order
(@+) :: Eq a => [Factor a] -> [Factor a] -> [Factor a]
fs1 @+ fs2 = fs1 @@+ reorder fs2 fs1

infixl 6 @@-
-- | Subtract corresponding exponents, assuming similar ordering
(@@-) :: Eq a => [Factor a] -> [Factor a] -> [Factor a]
[]             @@- fs2                       = negDim <$> fs2
fs1            @@- []                        = fs1
(F u1 e1 : t1) @@- (F u2 e2 : t2) | u1 == u2 = F u1 (e1 - e2) : t1 @@- t2
(h : t)        @@- fs2                       = h : (t @@- fs2)

infixl 6 @-
-- | Subtract corresponing exponents, preserving order
(@-) :: Eq a => [Factor a] -> [Factor a] -> [Factor a]
fs1 @- fs2 | fs1 == fs2 = []
           | otherwise  = fs1 @@- reorder fs2 fs1

infixl 7 @*
-- Multiply the exponents in a @[Factor]@ by a scalar
(@*) :: [Factor a] -> Integer -> [Factor a]
[]          @* _     = []
(F u e : t) @* power = F u (e * power) : (t @* power)

infixl 7 @/
-- Divide the exponents in a @[Factor]@ by a scalar
(@/) :: [Factor a] -> Integer -> [Factor a]
[]          @/ _     = []
(F u e : t) @/ power = F u (e `div` power) : (t @/ power)

infix 4 @~
-- | Check if two @[Factor]@s should be considered equal
(@~) :: Eq a => [Factor a] -> [Factor a] -> Bool
fs1 @~ fs2 = normalize (fs1 @- fs2) == []

-- | Negate a single 'Factor'
negDim :: Factor a -> Factor a
negDim (F u e) = F u (negate e)
