{-# OPTIONS_GHC -Wno-missing-methods #-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Units.Simple.Quantity
  ( Quantity (..)
  ) where

import GHC.TypeLits

import Units.Simple.Unit

-- |A numerical quantity with some associated unit.
-- Units are not created directly with this constructor,
-- but through the individual unit functions.
--
-- Examples:
--
-- >>> meter -- a unitary quantity associated to meters.
-- 1 m
-- >>> 2.5*ampere -- constructing through literal arithmetic
-- 2.5 A
-- >>> candela' 3.14 -- constructing through the unit constructors
-- 3.14 cd
-- >>> :set -XDataKinds
-- >>> 2 :: Quantity (SingleUnit 'Second) Rational
-- 2 % 1 s
--
-- Associated units are represented through a phantom parameter of the 'Units' kind synonym.
-- These are currently implemented as a type-level pair of lists representing the power
-- to which each unit is raised. Units can be inspected through 'showUnits'.
--
-- New constructors may be written by combining the provided ones, such as
--
-- >>> let newton = kilogram .* meter ./ (second .* second)
-- >>> 23*newton
-- 23.0 kg*m/s^2
-- <BLANKLINE>
-- >>> let g = 6.67408e-11 * newton .* (meter .* meter) ./ (kilogram .* kilogram)
-- >>> g -- gravitational constant
-- 6.67408e-11 m^3/kg*s^2
-- >>> let gravity m1 m2 r = g .* (m1 * kilogram) .* (m2 * kilogram) ./ (r*meter .* r*meter)
-- >>> let earth_mass = 5.972e24 * kilo gram
-- >>> let mars_mass = 6.417e23 * kilo gram
-- >>> let earth_radius = 6371 * kilo meter
-- >>> let mars_radius = 3389.5 * kilo meter
-- >>> let weight_on_earth mass = gravity mass earth_mass earth_radius
-- >>> let weight_on_mars mass = gravity mass mars_mass mars_radius
-- >>> weight_on_earth (80 * kilo gram)
-- 785.5719790179963 kg*m/s^2
-- >>> weight_on_mars (80 * kilo gram)
-- 298.22370259533704 kg*m/s^2
-- >>> weight_on_mars 1 / weight_on_earth 1
-- 0.3796261966575378 <adimensional>
newtype Quantity (us :: Units) a =
  Quantity {
    fromQuantity :: a -- ^Unwraps a 'Quantity', losing all unit information
  }
  deriving ( Eq
           , Ord
           , Enum
           , Num
           , Real
           , Integral
           , Fractional
           , Floating
           , Functor
  )

instance (KnownSymbol (UnitRepr us), Show a) => Show (Quantity us a) where
  show (Quantity x) = show x <> " " <> showUnits @us

instance Applicative (Quantity us) where
  pure = Quantity
  Quantity f <*> Quantity x = Quantity (f x)
