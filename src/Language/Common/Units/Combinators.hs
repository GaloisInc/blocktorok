{-|
Module      : Language.Common.Units.Combinators
Description : Functions to build new units/dimensions from old
Copyright   : (c) Galois, Inc. 2021
License     : N/A
Maintainer  : cphifer@galois.com
Stability   : experimental
Portability : N/A

Operators for building new unit values from old ones (e.g. multiplication)
-}

module Language.Common.Units.Combinators where

import Language.Common.Units.Units

infixl 7 |*
-- | Multiply two units to get another unit
(|*) :: Unit -> Unit -> Unit
(|*) = undefined
