{-# OPTIONS_GHC -Wno-missing-methods #-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Units.Simple.Quantity
  ( Quantity (..)
  ) where

import GHC.TypeLits

import Units.Simple.Unit

newtype Quantity (us :: Units) a = Quantity { fromQuantity :: a }
  deriving (Eq, Ord, Num)

instance (KnownSymbol (UnitRepr us), Show a) => Show (Quantity us a) where
  show (Quantity x) = show x <> " " <> showUnits @us
