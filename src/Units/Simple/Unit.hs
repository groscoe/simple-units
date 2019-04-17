{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Units.Simple.Unit where

import Data.Proxy
import GHC.TypeLits hiding (type (<=))

import Fcf hiding (type (<=))

import Units.Simple.Internals (Show', Intersperse, Sort, type (<=))

data Unit = Meter
          | Kilogram
          | Second
          | Ampere
          | Kelvin
          | Mole
          | Candela
  deriving (Eq, Ord, Show)

type Unit' = (Unit, Nat)
type Units = ([Unit'], [Unit'])

type family UnitSymbol (u :: Unit) :: Symbol
type instance UnitSymbol 'Meter = "m"
type instance UnitSymbol 'Second = "s"
type instance UnitSymbol 'Kilogram = "kg"
type instance UnitSymbol 'Ampere = "A"
type instance UnitSymbol 'Kelvin = "K"
type instance UnitSymbol 'Mole = "mol"
type instance UnitSymbol 'Candela = "cd"

data ShowUnit :: Unit' -> Exp Symbol
type instance Eval (ShowUnit '(u, n)) =
  If (Eval (TyEq n 1))
     (UnitSymbol u)
     (UnitSymbol u `AppendSymbol` "^" `AppendSymbol` Eval (Show' n))

type ShowUnitList (ul :: [Unit']) = Eval (Intersperse "*" =<< Map ShowUnit =<< Sort ul)

type UnitRepr' (num :: [Unit']) (denom :: [Unit']) =
  If (Eval (Eval (Null num) && Eval (Null denom)))
     "<adimensional>"
     (If (Eval (Null denom))
         (ShowUnitList num)
         (If (Eval (Null num))
              ("1/" `AppendSymbol` ShowUnitList denom)
              (ShowUnitList num `AppendSymbol` "/" `AppendSymbol` ShowUnitList denom)))

type UnitRepr (us :: Units) = UnitRepr' (Eval (Fst us)) (Eval (Snd us))

-- | A string representation of 'Units'. Useful for debugging.
showUnits :: forall us. KnownSymbol (UnitRepr us) => String
showUnits = symbolVal (Proxy @(UnitRepr us))

type instance Eval ((a :: Unit) <= (b :: Unit)) = Eval (UnitSymbol a <= UnitSymbol b)
