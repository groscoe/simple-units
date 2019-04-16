{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
module Units.Simple.Internals where

import GHC.TypeLits as TL

import Fcf


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
type instance Eval (Show' (n :: Nat)) = Eval (Guarded n
  '[ Flip (Fcf.<=) 9 ':= NatToDigit n
   , Otherwise ':= Show' (Eval (n / 10)) <> Show' (Eval (n % 10))
   ])


data LE :: k -> k -> Exp Bool
type instance Eval ((a :: Nat) `LE` (b :: Nat)) = a <=? b
type instance Eval ((a :: Symbol) `LE` (b :: Symbol)) = Eval (Guarded (CmpSymbol a b)
  '[ TyEq 'EQ ':= Pure 'True
   , TyEq 'LT ':= Pure 'True
   , Otherwise ':= Pure 'False
   ])
type instance Eval ('(a, _) `LE` '(b, _)) = Eval (a `LE` b)


data QSort :: [a] -> Exp [a]
type instance Eval (QSort '[]) = '[]
type instance Eval (QSort (pivot ': rest)) =
  Eval (Eval
        (Eval (QSort (Eval (Filter (Flip LE pivot) rest)))
               ++ '[pivot])
        ++ Eval (QSort (Eval (Filter (Not <=< Flip LE pivot) rest))))

data DeleteBy :: (a -> a-> Exp Bool) -> a -> [a] -> Exp [a]
type instance Eval (DeleteBy _ _ '[]) = '[]
type instance Eval (DeleteBy p x (y ': ys)) =
  If (Eval (x `p` y)) ys (y ': Eval (DeleteBy p x ys))

data Foldl :: (b -> a -> Exp b) -> b -> t a -> Exp b
type instance Eval (Foldl _ e '[]) = e
type instance Eval (Foldl f e (x ': xs)) = Eval (Foldl f (Eval (f e x)) xs)

