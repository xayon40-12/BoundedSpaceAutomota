module Vect where

import Data.Distributive
import Data.Functor.Rep
import Fin
import GHC.Exts (IsList, Item, fromList, toList)
import Nat

data Vect (n :: N) a where
  Nil :: Vect Z a
  (:>) :: a -> Vect n a -> Vect (S n) a

infixr 9 :>

deriving instance (Show a) => Show (Vect n a)

deriving instance (Eq a) => Eq (Vect n a)

deriving instance (Ord a) => Ord (Vect n a)

instance Functor (Vect n) where
  fmap :: (a -> b) -> Vect n a -> Vect n b
  fmap _ Nil = Nil
  fmap f (x :> xs) = f x :> fmap f xs

instance Distributive (Vect Z) where
  distribute = distributeRep

instance Representable (Vect Z) where
  type Rep (Vect Z) = Fin Z

  tabulate :: (Rep (Vect Z) -> a) -> Vect Z a
  tabulate _ = Nil

  index :: Vect Z a -> Rep (Vect Z) -> a
  index Nil _ = undefined

type Rv n = (Representable (Vect n), Rep (Vect n) ~ Fin n)

instance (Rv n) => Distributive (Vect (S n)) where
  distribute = distributeRep

instance (Rv n) => Representable (Vect (S n)) where
  type Rep (Vect (S n)) = Fin (S n)

  tabulate :: (Fin (S n) -> a) -> Vect (S n) a
  tabulate f = f FO :> tabulate (f . FS)

  index :: Vect (S n) a -> Rep (Vect (S n)) -> a
  index (x :> _) FO = x
  index (_ :> xs) (FS k) = index xs k

--TODO use Liquid haskell to enforce length [Item (Vect k a)] ~ k
instance IsList (Vect Z a) where
  type Item (Vect Z a) = a
  fromList [] = Nil
  fromList _ = error "Wrong list length."
  toList Nil = []

type Lv k a = (IsList (Vect k a), Item (Vect k a) ~ a)

--TODO use Liquid haskell to enforce length [Item (Vect k a)] ~ k
instance (Lv k a) => IsList (Vect (S k) a) where
  type Item (Vect (S k) a) = a
  fromList :: [Item (Vect (S k) a)] -> Vect (S k) a
  fromList [] = error "Wrong list length."
  fromList (x : xs) = x :> fromList xs
  toList :: Vect (S k) a -> [Item (Vect (S k) a)]
  toList (x :> xs) = x : toList xs
