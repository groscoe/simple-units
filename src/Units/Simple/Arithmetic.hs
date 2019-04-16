{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Units.Simple.Arithmetic
  ( (.+)
  , (.-)
  , (.*)
  , (./)
  ) where

import Data.Kind
import GHC.TypeLits as TL

import Fcf

import Units.Simple.Unit
import Units.Simple.Quantity
import Units.Simple.Internals (Foldl, DeleteBy)


data Lookup' :: [(k, b)] -> k -> Exp (Maybe b)
type instance Eval (Lookup' as a) = Eval (Lookup a as)

type InList (as :: [(k, b)]) = IsJust <=< Lookup' as

type And (f :: a -> Exp Bool) (as :: [a]) = Foldr (&&) 'True (Eval (Map f as))

type AllInSecond (l1 :: [(k, a)]) (l2 :: [(k, a)]) = And (InList l2 <=< Fst) l1

type SameMaps (l1 :: [(k, a)]) (l2 :: [(k, a)]) = Eval (AllInSecond l1 l2) && Eval (AllInSecond l2 l1)

type SameUnits' (u1 :: Units) (u2 :: Units) = Eval (SameMaps (Eval (Fst u1)) (Eval (Fst u2)))
                                            && Eval (SameMaps (Eval (Snd u1)) (Eval (Snd u2)))

type family SameUnits (u1 :: Units) (u2 :: Units) :: Constraint where
  SameUnits u1 u2 = If (Eval (SameUnits' u1 u2))
                    (Eval (SameUnits' u1 u2) ~ 'True)
                    (TypeError
                     ('Text "Unit mismatch: "
                     ':<>: 'Text (UnitRepr u1)
                     ':<>: 'Text " and "
                     ':<>: 'Text (UnitRepr u2)))

type CleanUnitList = Filter (Not <=< TyEq 0 <=< Snd)
type CleanUnits us = (CleanUnitList *** CleanUnitList) us

data CancelUnit :: Unit' -> Unit' -> Exp (Unit', Unit')
type instance Eval (CancelUnit '(a, m) '(b, n)) =
  If (Eval (LiftM2 (&&) (TyEq a b) (n >= m)))
     '( '(a, 0), '(b, n TL.- m))
     '( '(a, m TL.- n), '(b, 0))

type GetMatchingUnits l1 l2 =
  (Filter (Flip UnitInList l2) *** Filter (Flip UnitInList l1)) '(l1, l2)
type CancelMatchingUnitLists l1 l2 =
  Unzip (Uncurry (ZipWith CancelUnit) (Eval (GetMatchingUnits l1 l2)))
type CancelUnitLists l1 l2 =
  ((++) (Eval (l1 \\ l2)) *** (++) (Eval (l2 \\ l1))) =<< CancelMatchingUnitLists l1 l2
type CancelUnits (a :: Units) = CancelUnitLists (Eval (Fst a)) (Eval (Snd a))

data SameUnit :: Unit' -> Unit' -> Exp Bool
type instance Eval (SameUnit a b) = Eval (LiftM2 TyEq (Fst a) (Fst b))
type (\\) = Foldl (Flip (DeleteBy SameUnit))

data MergeUnit :: Unit' -> Unit' -> Exp Unit'
type instance Eval (MergeUnit '(a, m) '(b, n)) =
  If (Eval (TyEq a b))
     '(a, (TL.+) m n)
     '(b, n)

data UnitInList :: Unit' -> [Unit'] -> Exp Bool
type instance Eval (UnitInList '(u, n) '[]) = 'False
type instance Eval (UnitInList '(u, n) ('(v, _) ': xs)) = Eval (Eval (TyEq u v) || Eval (UnitInList '(u, n) xs))

data MergeUnitFromList :: u -> [us] -> Exp [us]
type instance Eval (MergeUnitFromList u us) = Eval (Map (MergeUnit u) us)

type MergeUnitLists (us1 :: [Unit']) (us2 :: [Unit']) = Foldr MergeUnitFromList us2 us1
data MergeUnits' :: Units -> Units -> Exp Units
type instance Eval (MergeUnits' '(num1, denom1) '(num2, denom2)) =
  Eval (CancelUnits
    '( Eval (Eval (MergeUnitLists num1 num2)     ++ Eval (Filter (Not <=< Flip UnitInList num2) num1))
     , Eval (Eval (MergeUnitLists denom1 denom2) ++ Eval (Filter (Not <=< Flip UnitInList denom2) denom1))))

type MergeUnits us1 us2 = Eval (CleanUnits (Eval (MergeUnits' us1 us2)))

data Recip' :: Units -> Exp Units
type instance Eval (Recip' '(num, denom)) = '(denom, num)
type Recip a = Eval (Recip' a)


(.+) :: (SameUnits u1 u2, Num a) => Quantity u1 a -> Quantity u2 a -> Quantity u1 a
Quantity x .+ Quantity y = Quantity (x + y)
infixl 5 .+

(.-) :: (SameUnits u1 u2, Num a) => Quantity u1 a -> Quantity u2 a -> Quantity u1 a
Quantity x .- Quantity y = Quantity (x - y)
infixl 5 .-

(.*) :: Num a => Quantity us1 a -> Quantity us2 a -> Quantity (MergeUnits us1 us2) a
Quantity x .* Quantity y = Quantity (x * y)
infixl 6 .*

(./) :: Fractional a => Quantity us1 a -> Quantity us2 a -> Quantity (MergeUnits us1 (Recip us2)) a
Quantity x ./ Quantity y = Quantity (x / y)
infixl 6 ./

-- -- NOTE: Work in progress - exponentiation
-- type ExpUnitList (n :: Nat) (us :: [Unit']) = Map (Map ((Fcf.*) n)) us
-- type ExpUnits (n :: Nat) (u :: Units) = '(Eval (ExpUnitList n (Eval (Fst u))), Eval (Snd u))

-- expQ :: forall n a u. (KnownNat n, Num a)
--      => Quantity u a
--      -> Quantity (ExpUnits n u) a
-- expQ (Quantity x) = Quantity (x ^ natVal (Proxy @n))
