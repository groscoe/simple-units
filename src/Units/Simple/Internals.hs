{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Units.Simple.Internals where

import GHC.TypeLits hiding (type (<=))
import qualified GHC.TypeLits as TL

import Fcf hiding (type (<=))
import qualified Fcf


data ApSep :: Symbol -> Symbol -> Symbol -> Exp Symbol
type instance Eval (ApSep sep x xs) = sep `AppendSymbol` x `AppendSymbol` xs

type Intersperse' sep xs = Foldr (ApSep sep) "" xs

data Intersperse :: Symbol -> [Symbol] -> Exp Symbol
type instance Eval (Intersperse _ '[]) = ""
type instance Eval (Intersperse sep (x ': xs)) = x `AppendSymbol` Eval (Intersperse' sep xs)


data NatToDigit :: Nat -> Exp Symbol
type instance Eval (NatToDigit 0) = "0"
type instance Eval (NatToDigit 1) = "1"
type instance Eval (NatToDigit 2) = "2"
type instance Eval (NatToDigit 3) = "3"
type instance Eval (NatToDigit 4) = "4"
type instance Eval (NatToDigit 5) = "5"
type instance Eval (NatToDigit 6) = "6"
type instance Eval (NatToDigit 7) = "7"
type instance Eval (NatToDigit 8) = "8"
type instance Eval (NatToDigit 9) = "9"

data (<>) :: Exp a -> Exp a -> Exp a
type instance Eval ((a :: Exp Symbol) <> (b :: Exp Symbol)) = Eval a `AppendSymbol` Eval b

data (/) :: Nat -> Nat -> Exp Nat
type instance Eval (a / b) = a `TL.Div` b

data (%) :: Nat -> Nat -> Exp Nat
type instance Eval (a % b) = a `TL.Mod` b

data Show' :: a -> Exp Symbol
type instance Eval (Show' (n :: Nat)) =
  Eval (
    If (Eval ((Fcf.<=) n 9))
       (NatToDigit n)
       (Show' (Eval (n / 10)) <> Show' (Eval (n % 10)))
  )


data (<=) :: k -> k -> Exp Bool
type instance Eval ((a :: Nat) <= (b :: Nat)) = a <=? b
type instance Eval ((a :: Symbol) <= (b :: Symbol)) =
  Eval (
    If (Eval (TyEq 'GT (CmpSymbol a b)))
       (Pure 'False)
       (Pure 'True)
  )

type instance Eval ('(a, _) <= '(b, _)) = Eval (a <= b)


data Sort :: [a] -> Exp [a]
type instance Eval (Sort '[]) = '[]
type instance Eval (Sort (pivot ': rest)) =
  Eval (Eval
        (Eval (Sort (Eval (Filter (Flip (<=) pivot) rest)))
               ++ '[pivot])
        ++ Eval (Sort (Eval (Filter (Not <=< Flip (<=) pivot) rest))))

data DeleteBy :: (a -> a-> Exp Bool) -> a -> [a] -> Exp [a]
type instance Eval (DeleteBy _ _ '[]) = '[]
type instance Eval (DeleteBy p x (y ': ys)) =
  If (Eval (x `p` y)) ys (y ': Eval (DeleteBy p x ys))

data Foldl :: (b -> a -> Exp b) -> b -> t a -> Exp b
type instance Eval (Foldl _ e '[]) = e
type instance Eval (Foldl f e (x ': xs)) = Eval (Foldl f (Eval (f e x)) xs)

