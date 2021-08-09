{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Language.Common.Units.SI
Description : Definitions of SI dimensions / units
Copyright   : (c) Galois, Inc. 2021
License     : N/A
Maintainer  : cphifer@galois.com
Stability   : experimental
Portability : N/A

SI units defined using the other machinery.
-}

module Language.Common.Units.SI where

import Prelude hiding (length)

import Language.Common.Units.Combinators
import Language.Common.Units.Dimensions
import Language.Common.Units.Units

-- Prefixes
deca :: Rational
deca = 1e1

hecto :: Rational
hecto = 1e2

kilo :: Rational
kilo = 1e3

mega :: Rational
mega = 1e6

giga :: Rational
giga = 1e9

tera :: Rational
tera = 1e12

peta :: Rational
peta = 1e15

exa :: Rational
exa = 1e18

zetta :: Rational
zetta = 1e21

yotta :: Rational
yotta = 1e24

deci :: Rational
deci = 1e-1

centi :: Rational
centi = 1e-2

milli :: Rational
milli = 1e-3

micro :: Rational
micro = 1e-6

nano :: Rational
nano = 1e-9

pico :: Rational
pico = 1e-12

femto :: Rational
femto = 1e-15

atto :: Rational
atto = 1e-18

zepto :: Rational
zepto = 1e-21

yocto :: Rational
yocto = 1e-24


-- Base dimensions
length :: Dimension
length = mkDimension "Length"

mass :: Dimension
mass = mkDimension "Mass"

time :: Dimension
time = mkDimension "Time"

current :: Dimension
current = mkDimension "Current"

temperature :: Dimension
temperature = mkDimension "Temperature"

amountOfSubstance :: Dimension
amountOfSubstance = mkDimension "AmountOfSubstance"

luminousIntensity :: Dimension
luminousIntensity = mkDimension "LuminousIntensity"


-- Derived dimensions
area :: Dimension
area                = length            |^ 2

volume :: Dimension
volume              = length            |^ 3

velocity :: Dimension
velocity            = length            |/ time

acceleration :: Dimension
acceleration        = velocity          |/ time

wavenumber :: Dimension
wavenumber          = length            |^ (negate 1)

density :: Dimension
density             = mass              |/ volume

surfaceDensity :: Dimension
surfaceDensity      = mass              |/ area

specificVolume :: Dimension
specificVolume      = volume            |/ mass

currentDensity :: Dimension
currentDensity      = current           |/ area

magneticStrength :: Dimension
magneticStrength    = current           |/ length

concentration :: Dimension
concentration       = amountOfSubstance |/ volume

luminance :: Dimension
luminance           = luminousIntensity |/ area


frequency :: Dimension
frequency           = time              |^ (negate 1)

force :: Dimension
force               = mass              |* acceleration

pressure :: Dimension
pressure            = force             |/ area

energy :: Dimension
energy              = force             |* length

power :: Dimension
power               = energy            |/ time

charge :: Dimension
charge              = current           |* time

electricPotential :: Dimension
electricPotential   = power             |/ current

capacitance :: Dimension
capacitance         = charge            |/ electricPotential

resistance :: Dimension
resistance          = electricPotential |/ current

conductance :: Dimension
conductance         = current           |/ electricPotential

magneticFlux :: Dimension
magneticFlux        = electricPotential |* time

magneticFluxDensity :: Dimension
magneticFluxDensity = magneticFlux      |/ area

inductance :: Dimension
inductance          = magneticFlux      |/ current

luminousFlux :: Dimension
luminousFlux        = luminousIntensity

illuminance :: Dimension
illuminance         = luminousIntensity |/ area

kerma :: Dimension
kerma               = area              |/ (time |^ 2)

catalyticActivity :: Dimension
catalyticActivity   = amountOfSubstance |/ time


momentum :: Dimension
momentum            = mass              |* velocity


-- Canonical units
meter :: Unit
meter = mkCanonicalUnit "Meter" length (Just "m")

gram :: Unit
gram = mkCanonicalUnit "Gram" mass (Just "g")

second :: Unit
second = mkCanonicalUnit "Second" time (Just "s")

ampere :: Unit
ampere = mkCanonicalUnit "Ampere" current (Just "A")

kelvin :: Unit
kelvin = mkCanonicalUnit "Kelvin" temperature (Just "K")

mole :: Unit
mole = mkCanonicalUnit "Mole" amountOfSubstance (Just "mol")

candela :: Unit
candela = mkCanonicalUnit "Candela" luminousIntensity (Just "cd")


-- Derived units
minute :: Unit
minute = mkDerivedUnit "Minute" second 60 (Just "min")

hour :: Unit
hour = mkDerivedUnit "Hour" minute 60 (Just "h")

hertz :: Unit
hertz = mkDerivedUnit "Hertz" (number ||/ second) 1 (Just "Hz")

liter :: Unit
liter = mkDerivedUnit "Liter" ((centi ||@ meter) ||^ 3) 1000 (Just "L")

newton :: Unit
newton = mkDerivedUnit "Newton" (gram ||* meter ||/ (second ||^ 2)) 1000 (Just "N")

pascal :: Unit
pascal = mkDerivedUnit "Pascal" (newton ||/ (meter ||^ 2)) 1 (Just "Pa")

joule :: Unit
joule = mkDerivedUnit "Joule" (newton ||* meter) 1 (Just "J")

watt :: Unit
watt = mkDerivedUnit "Watt" (joule ||/ second) 1 (Just "W")

coulomb :: Unit
coulomb = mkDerivedUnit "Coulomb" (ampere ||* second) 1 (Just "C")

volt :: Unit
volt = mkDerivedUnit "Volt" (watt ||/ ampere) 1 (Just "V")

farad :: Unit
farad = mkDerivedUnit "Farad" (coulomb ||/ volt) 1 (Just "F")

ohm :: Unit
ohm = mkDerivedUnit "Ohm" (volt ||/ ampere) 1 (Just "Ω")

siemens :: Unit
siemens = mkDerivedUnit "Siemens" (ampere ||/ volt) 1 (Just "S")

weber :: Unit
weber = mkDerivedUnit "Weber" (volt ||* second) 1 (Just "Wb")

tesla :: Unit
tesla = mkDerivedUnit "Tesla" (weber ||/ (meter ||^ 2)) 1 (Just "T")

henry :: Unit
henry = mkDerivedUnit "Henry" (weber ||/ ampere) 1 (Just "H")

lumen :: Unit
lumen = mkDerivedUnit "Lumen" candela 1 (Just "lm")

lux :: Unit
lux = mkDerivedUnit "Lux" (lumen ||/ (meter ||^ 2)) 1 (Just "lx")

becquerel :: Unit
becquerel = mkDerivedUnit "Becquerel" (number ||/ second) 1 (Just "Bq")

gray :: Unit
gray = mkDerivedUnit "Gray" ((meter ||^ 2) ||/ (second ||^ 2)) 1 (Just "Gy")

sievert :: Unit
sievert = mkDerivedUnit "Sievert" ((meter ||^ 2) ||/ (second ||^ 2)) 1 (Just "Sv")

katal :: Unit
katal = mkDerivedUnit "Katal" (mole ||/ second) 1 (Just "kat")


hectare :: Unit
hectare = mkDerivedUnit "Hectare" (meter ||^ 2) 10000 (Just "ha")


ton :: Unit
ton = mkDerivedUnit "Ton" (kilo ||@ gram) 1000 (Just "t")
