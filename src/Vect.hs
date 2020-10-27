module Vect where
    
import Nat
import Fin
import Store
import Data.Functor.Rep
import Data.Distributive

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

type Rv n = (Representable (Vect n), Rep (Vect n) ~ Fin n)
instance (Rv n) => Distributive (Vect (S n)) where
    distribute = distributeRep

instance (Rv n) => Representable (Vect (S n)) where
    type Rep (Vect (S n)) = Fin (S n)

    tabulate :: (Fin (S n) -> a) -> Vect (S n) a
    tabulate f = f FO :> tabulate (f . FS)

    index :: Vect (S n) a -> Rep (Vect (S n)) -> a
    index (x:>_) FO = x
    index (_:>xs) (FS k) = index xs k
