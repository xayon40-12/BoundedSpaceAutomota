{-# LANGUAGE
--TypeInType,
--TypeApplications,
TypeFamilies,
--ScopedTypeVariables,
DataKinds,
GADTs,
UndecidableInstances,
--TypeOperators,
--AllowAmbiguousTypes,
--TypeSynonymInstances,
--MultiParamTypeClasses,
FlexibleContexts,
FlexibleInstances,
StandaloneDeriving,
InstanceSigs,
PolyKinds
#-}

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Lib where

import Data.Functor.Rep
import Data.Distributive

data N = Z | S N deriving (Show, Eq, Ord)
instance Enum N where
    toEnum 0 = Z
    toEnum i = S . toEnum $ i-1
    fromEnum Z = 0
    fromEnum (S n) = 1 + fromEnum n

type family Nadd a b where
    Nadd Z Z = Z
    Nadd (S n) Z = S n
    Nadd Z (S n) = S n
    Nadd (S n) (S m) = S (Nadd n (S m))

type family Nmul a b where
    Nmul Z Z = Z
    Nmul (S n) Z = Z
    Nmul Z (S n) = Z
    Nmul (S Z) (S Z) = S Z
    Nmul (S n) (S m) = Nadd (S m) (Nmul n (S m))

type N0 = Z
type N1 = S N0
type N2 = S N1
type N3 = S N2
type N4 = S N3
type N5 = S N4
type N6 = S N5
type N7 = S N6
type N8 = S N7
type N9 = S N8
type N10 = Nadd N5 N5
type N100 = Nmul N10 N10
type N1000 = Nmul N10 N100
type N10000 = Nmul N10 N1000

data Fin (n :: N) where
    FO :: Fin (S n) -- One element
    FS :: Fin n -> Fin (S n) -- n + 1 element
instance (Enum (Fin (S n))) => Show (Fin (S n)) where
    show n = show (fromEnum n)
deriving instance Eq (Fin n)
deriving instance Ord (Fin n)

instance Enum (Fin (S Z)) where
    toEnum :: Int -> (Fin (S Z))
    toEnum 1 = FO
    toEnum i = error "Index out of bound."
    fromEnum :: (Fin (S Z)) -> Int
    fromEnum FO = 1
    fromEnum (FS _) = undefined -- Not rechable as Fin (S Z) is inhabited only by FO

instance (Enum (Fin (S n))) => Enum (Fin (S (S n))) where
    toEnum 1 = FO
    toEnum i = FS . toEnum $ i-1
    fromEnum FO = 1
    fromEnum (FS n) = 1 + fromEnum n

instance Bounded (Fin (S Z)) where
    minBound = FO
    maxBound = FO
instance (Bounded (Fin (S n))) => Bounded (Fin (S (S n))) where
    minBound = FO
    maxBound = FS maxBound


data Vect (n :: N) a where
    Nil :: Vect Z a
    (:>) :: a -> Vect n a -> Vect (S n) a
infixr :>
deriving instance (Show a) => Show (Vect n a)
deriving instance (Eq a) => Eq (Vect n a)
deriving instance (Ord a) => Ord (Vect n a)

instance Functor (Vect n) where
    fmap :: (a -> b) -> Vect n a -> Vect n b
    fmap f Nil = Nil
    fmap f (x:>xs) = f x :> fmap f xs

instance Distributive (Vect Z) where
    distribute = distributeRep

instance Representable (Vect Z) where
    type Rep (Vect Z) = Fin Z

    tabulate :: (Rep (Vect Z) -> a) -> Vect Z a
    tabulate f = Nil

    index :: Vect Z a -> Rep (Vect Z) -> a
    index Nil _ = undefined

instance (Representable (Vect n), Rep (Vect n) ~ Fin n) => Distributive (Vect (S n)) where
    distribute = distributeRep

instance (Representable (Vect n), Rep (Vect n) ~ Fin n) => Representable (Vect (S n)) where
    type Rep (Vect (S n)) = Fin (S n)

    tabulate :: (Fin (S n) -> a) -> Vect (S n) a
    tabulate f = f FO :> tabulate (f . FS)

    index :: Vect (S n) a -> Rep (Vect (S n)) -> a
    index (x:>_) FO = x
    index (_:>xs) (FS k) = index xs k
