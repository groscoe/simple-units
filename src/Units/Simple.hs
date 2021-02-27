{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module Units.Simple
  (-- *The @Quantity@ type
    Quantity ()
  , fromQuantity
  , Unit (..)
  , Units
  , SingleUnit
  , UnitRepr
  , showUnits

  -- * Basic arithmetic with quantities
  , (.+)
  , (.-)
  , (.*)
  , (./)

  -- *Base SI Units
  -- |Smart constructors for quantities in all the base SI Units, plus grams
  -- (see "SI Prefixes"). The constructors are provided in both unitary
  -- (non-ticked) and function (ticked) forms.
  --
  -- Examples:
  --
  -- >>> 273.0*kelvin -- unitary form
  -- 273.0 K
  -- >>> kelvin' 273.0 -- function form
  -- 273.0 K
  , adim
  , adim'
  , meter
  , meter'
  , kilogram
  , kilogram'
  , gram
  , gram'
  , second
  , second'
  , ampere
  , ampere'
  , kelvin
  , kelvin'
  , mole
  , mole'
  , candela
  , candela'

  -- *SI Prefixes
  -- |The 20 base SI prefixes representing powers of 10.
  -- Reference: <https://physics.nist.gov/cuu/Units/prefixes.html>
  --
  -- __Note:__ Though smart constructors for kilograms are provided, it is
  -- recommended to use the prefixes with the 'gram' and 'gram'' constructors.
  , yocto
  , yocto'
  , zepto
  , zepto'
  , atto
  , atto'
  , femto
  , femto'
  , pico
  , pico'
  , nano
  , nano'
  , micro
  , micro'
  , milli
  , milli'
  , centi
  , centi'
  , deci
  , deci'
  , deka
  , deka'
  , hecto
  , hecto'
  , kilo
  , kilo'
  , mega
  , mega'
  , giga
  , giga'
  , tera
  , tera'
  , peta
  , peta'
  , exa
  , exa'
  , zetta
  , zetta'
  , yotta
  , yotta'
  ) where


import Units.Simple.Quantity
import Units.Simple.Arithmetic
import Units.Simple.Unit

type SingleUnit (u :: Unit) = '( '[ '(u, 1)], '[])
type Adimensional = '( '[], '[])

-- | A 'Quantity' with no associated dimension. Can be multiplied or divided
-- by any other 'Quantity', but can only be added to or subtracted from
-- another adimensional 'Quantity'.
--
-- >>> 2*adim + 4*adim
-- 6 <adimensional>
-- >>> adim .+ meter
-- <BLANKLINE>
-- <interactive>:79:1-13: error:
--     • Unit mismatch: <adimensional> and m
--     • In the expression: adim .+ meter
--       In an equation for ‘it’: it = adim .+ meter
adim :: Num a => Quantity Adimensional a
adim = Quantity 1

-- SI units
meter :: Num a => Quantity (SingleUnit 'Meter) a
meter = Quantity 1

kilogram :: Num a => Quantity (SingleUnit 'Kilogram) a
kilogram = Quantity 1

-- |A constructor for 1/1000th of a kilogram, to use with SI prefixes.
gram :: Fractional a => Quantity (SingleUnit 'Kilogram) a
gram = Quantity 1e-3

second :: Num a => Quantity (SingleUnit 'Second) a
second = Quantity 1

ampere :: Num a => Quantity (SingleUnit 'Ampere) a
ampere = Quantity 1

kelvin :: Num a => Quantity (SingleUnit 'Kelvin) a
kelvin = Quantity 1

mole :: Num a => Quantity (SingleUnit 'Mole) a
mole = Quantity 1

candela :: Num a => Quantity (SingleUnit 'Candela) a
candela = Quantity 1

adim' :: Num a => a -> Quantity Adimensional a
adim' = Quantity

meter' :: Num a => a -> Quantity (SingleUnit 'Meter) a
meter' = Quantity

kilogram' :: Num a => a -> Quantity (SingleUnit 'Kilogram) a
kilogram' = Quantity

-- |A constructor for 1/1000th of a kilogram, to use with SI prefixes.
gram' :: Fractional a => a -> Quantity (SingleUnit 'Kilogram) a
gram' = (*1e-3) . Quantity

second' :: Num a => a -> Quantity (SingleUnit 'Second) a
second' = Quantity

ampere' :: Num a => a -> Quantity (SingleUnit 'Ampere) a
ampere' = Quantity

kelvin' :: Num a => a -> Quantity (SingleUnit 'Kelvin) a
kelvin' = Quantity

mole' :: Num a => a -> Quantity (SingleUnit 'Mole) a
mole' = Quantity

candela' :: Num a => a -> Quantity (SingleUnit 'Candela) a
candela' = Quantity

-- SI prefixes
yocto :: Fractional a => Quantity us a -> Quantity us a
yocto = (* 1e-24)

yocto' :: Fractional a => (a -> Quantity us a) -> a -> Quantity us a
yocto' constructor = yocto . constructor

zepto :: Fractional a => Quantity us a -> Quantity us a
zepto = (* 1e-21)

zepto' :: Fractional a => (a -> Quantity us a) -> a -> Quantity us a
zepto' constructor = zepto . constructor

atto :: Fractional a => Quantity us a -> Quantity us a
atto = (* 1e-18)

atto' :: Fractional a => (a -> Quantity us a) -> a -> Quantity us a
atto' constructor = atto . constructor

femto :: Fractional a => Quantity us a -> Quantity us a
femto = (* 1e-15)

femto' :: Fractional a => (a -> Quantity us a) -> a -> Quantity us a
femto' constructor = femto . constructor

pico :: Fractional a => Quantity us a -> Quantity us a
pico = (* 1e-12)

pico' :: Fractional a => (a -> Quantity us a) -> a -> Quantity us a
pico' constructor = pico . constructor

nano :: Fractional a => Quantity us a -> Quantity us a
nano = (* 1e-9)

nano' :: Fractional a => (a -> Quantity us a) -> a -> Quantity us a
nano' constructor = nano . constructor

micro :: Fractional a => Quantity us a -> Quantity us a
micro = (* 1e-6)

micro' :: Fractional a => (a -> Quantity us a) -> a -> Quantity us a
micro' constructor = micro . constructor

milli :: Fractional a => Quantity us a -> Quantity us a
milli = (* 1e-3)

milli' :: Fractional a => (a -> Quantity us a) -> a -> Quantity us a
milli' constructor = milli . constructor

centi :: Fractional a => Quantity us a -> Quantity us a
centi = (* 1e-2)

centi' :: Fractional a => (a -> Quantity us a) -> a -> Quantity us a
centi' constructor = centi . constructor

deci :: Fractional a => Quantity us a -> Quantity us a
deci = (* 1e-1)

deci' :: Fractional a => (a -> Quantity us a) -> a -> Quantity us a
deci' constructor = deci . constructor

deka :: Num a => Quantity us a -> Quantity us a
deka = (* 10)

deka' :: Num a => (a -> Quantity us a) -> a -> Quantity us a
deka' constructor = deka . constructor

hecto :: Num a => Quantity us a -> Quantity us a
hecto = (* 100)

hecto' :: Num a => (a -> Quantity us a) -> a -> Quantity us a
hecto' constructor = hecto . constructor

kilo :: Num a => Quantity us a -> Quantity us a
kilo = (* 1000)

kilo' :: Num a => (a -> Quantity us a) -> a -> Quantity us a
kilo' constructor = kilo . constructor

mega :: Num a => Quantity us a -> Quantity us a
mega = (* 1000000)

mega' :: Num a => (a -> Quantity us a) -> a -> Quantity us a
mega' constructor = mega . constructor

giga :: Num a => Quantity us a -> Quantity us a
giga = (* 1000000000)

giga' :: Num a => (a -> Quantity us a) -> a -> Quantity us a
giga' constructor = giga . constructor

tera :: Num a => Quantity us a -> Quantity us a
tera = (* 1000000000000)

tera' :: Num a => (a -> Quantity us a) -> a -> Quantity us a
tera' constructor = tera . constructor

peta :: Num a => Quantity us a -> Quantity us a
peta = (* 1000000000000000)

peta' :: Num a => (a -> Quantity us a) -> a -> Quantity us a
peta' constructor = peta . constructor

exa :: Num a => Quantity us a -> Quantity us a
exa = (* 1000000000000000000)

exa' :: Num a => (a -> Quantity us a) -> a -> Quantity us a
exa' constructor = exa . constructor

zetta :: Num a => Quantity us a -> Quantity us a
zetta = (* 1000000000000000000000)

zetta' :: Num a => (a -> Quantity us a) -> a -> Quantity us a
zetta' constructor = zetta . constructor

yotta :: Num a => Quantity us a -> Quantity us a
yotta = (* 1000000000000000000000000)

yotta' :: Num a => (a -> Quantity us a) -> a -> Quantity us a
yotta' constructor = yotta . constructor
