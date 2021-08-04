{-|
Module      : Language.Common.Units.SI
Description : SI units
Copyright   : (c) Galois, Inc. 2021
License     : N/A
Maintainer  : cphifer@galois.com
Stability   : experimental
Portability : N/A

Definition of base units and expression language for putting them together.
-}

module Language.Common.Units.SI
  ( BaseUnit(..)
  , Prefix(..)
  , Unit(..)
  , multiplier
  ) where

-- | The seven base SI units
data BaseUnit
  = Meter
  | Gram
  | Second
  | Ampere
  | Kelvin
  | Mole
  | Candela
  deriving (Eq)

instance Show BaseUnit where
  show Meter   = "m"
  show Gram    = "g"
  show Second  = "s"
  show Ampere  = "A"
  show Kelvin  = "K"
  show Mole    = "mol"
  show Candela = "cd"

-- | Common prefixes for SI units
data Prefix
  = Deca  -- 10^1
  | Hecto -- 10^2
  | Kilo  -- 10^3
  | Mega  -- 10^6
  | Giga  -- 10^9
  | Tera  -- 10^12
  | Peta  -- 10^15
  | Exa   -- 10^18
  | Zetta -- 10^21
  | Yotta -- 10^24
  | Deci  -- 10^-1
  | Centi -- 10^-2
  | Milli -- 10^-3
  | Micro -- 10^-6
  | Nano  -- 10^-9
  | Pico  -- 10^-12
  | Femto -- 10^-15
  | Atto  -- 10^-18
  | Zepto -- 10^-21
  | Yocto -- 10^-24
  deriving (Eq)

instance Show Prefix where
  show Deca  = "da"
  show Hecto = "h"
  show Kilo  = "k"
  show Mega  = "M"
  show Giga  = "G"
  show Tera  = "T"
  show Peta  = "P"
  show Exa   = "E"
  show Zetta = "Z"
  show Yotta = "Y"
  show Deci  = "d"
  show Centi = "c"
  show Milli = "m"
  show Micro = "μ"
  show Nano  = "n"
  show Pico  = "p"
  show Femto = "f"
  show Atto  = "a"
  show Zepto = "z"
  show Yocto = "y"

-- | Return the multiplier implied by a prefix
multiplier :: Fractional f => Prefix -> f
multiplier Deca  = 1e1
multiplier Hecto = 1e2
multiplier Kilo  = 1e3
multiplier Mega  = 1e6
multiplier Giga  = 1e9
multiplier Tera  = 1e12
multiplier Peta  = 1e15
multiplier Exa   = 1e18
multiplier Zetta = 1e21
multiplier Yotta = 1e24
multiplier Deci  = 1e-1
multiplier Centi = 1e-2
multiplier Milli = 1e-3
multiplier Micro = 1e-6
multiplier Nano  = 1e-9
multiplier Pico  = 1e-12
multiplier Femto = 1e-15
multiplier Atto  = 1e-18
multiplier Zepto = 1e-21
multiplier Yocto = 1e-24

-- | Unit expressions
data Unit
  = Unity
  | Unit (Maybe Prefix) BaseUnit
  | Unit :* Unit
  | Unit :/ Unit
  | Unit :^ Unit
  deriving (Eq)
