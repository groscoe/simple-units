{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
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
  -- |Smart constructors for quantities in all the base SI Units.
  -- The constructors are provided in both unitary (non-ticked) and
  -- function (ticked) forms.
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
  ) where


import Units.Simple.Quantity
import Units.Simple.Arithmetic
import Units.Simple.Unit

type SingleUnit (u :: Unit) = '( '[ '(u, 1)], '[])
type Adimensional = '( '[], '[])

-- | A quantity with no associated dimension. Can be multiplied or divided
-- by any other quantity, but can only be added to or subtracted from
-- other adimensional quantities.
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

meter :: Num a => Quantity (SingleUnit 'Meter) a
meter = Quantity 1

kilogram :: Num a => Quantity (SingleUnit 'Kilogram) a
kilogram = Quantity 1

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
