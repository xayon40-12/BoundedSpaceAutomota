module Store where
    
import Data.Functor.Rep
import Control.Comonad

type R f rep= (Representable f, Rep f ~ rep)

data Store :: (* -> *) -> * -> * -> * where
    MkStore :: rep -> f a -> Store f rep a

deriving instance (Show a, Show rep, Show (f a)) => Show (Store f rep a)

pos :: Store f rep a -> rep
pos (MkStore rep fa) = rep

peek :: R f rep => rep -> Store f rep a -> a
peek rep (MkStore _ fa) = index fa rep

peeks :: R f rep => (rep -> rep) -> Store f rep a -> a
peeks f (MkStore rep fa) = index fa (f rep)

seek :: R f rep => rep -> Store f rep a -> Store f rep a
seek rep (MkStore _ fa) = MkStore rep fa

seeks :: R f rep => (rep -> rep) -> Store f rep a -> Store f rep a
seeks func (MkStore rep fa) = MkStore (func rep) fa

experiment :: (R f rep, Functor g) => (rep -> g rep) -> Store f rep a -> g a
experiment f s = fmap (`peek` s) (f (pos s))

instance (Functor f) => Functor (Store f rep) where
    fmap f (MkStore rep fa) = MkStore rep (fmap f fa)

instance (R f rep) => Comonad (Store f rep) where
    extract :: Store f rep a -> a
    extract (MkStore rep fa) = index fa rep

    extend :: (Store f rep a -> b) -> Store f rep a -> Store f rep b
    extend func (MkStore rep fa) = MkStore rep (tabulate (\rep' -> func (MkStore rep' fa)))
