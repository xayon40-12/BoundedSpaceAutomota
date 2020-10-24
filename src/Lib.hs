{-# LANGUAGE
--TypeInType,
--TypeApplications,
TypeFamilies,
--ScopedTypeVariables,
DataKinds,
GADTs,
--UndecidableInstances,
--TypeOperators,
--AllowAmbiguousTypes,
--TypeSynonymInstances,
--MultiParamTypeClasses,
FlexibleContexts,
FlexibleInstances,
StandaloneDeriving,
InstanceSigs
#-}

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Lib where

import Data.Functor.Rep
import Data.Distributive

data N = Z | S N
type N0 = Z
type N1 = S N0
type N2 = S N1
type N3 = S N2

data Vect (n :: N) a where
    Nil :: Vect Z a
    (:>) :: a -> Vect n a -> Vect (S n) a
infixr :>
deriving instance (Show a) => Show (Vect n a)

data Fin (n :: N) where
    FZ :: Fin (S n)
    FS :: Fin n -> Fin (S n)

ft :: Fin n -> Int
ft FZ = 0
ft (FS n) = 1 + ft n

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
    tabulate f = f FZ :> tabulate (f . FS)

    index :: Vect (S n) a -> Rep (Vect (S n)) -> a
    index (x:>_) FZ = x
    index (_:>xs) (FS k) = index xs k

test = tabulate ft :: Vect N3 Int
