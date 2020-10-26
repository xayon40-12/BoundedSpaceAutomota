module Rule where

import Vect
import Fin
import Nat
import Store

initialStore :: Vect (Nadd N3 k) Bool -> Store (Vect (Nadd N3 k)) (Fin (Nadd N3 k)) Bool
initialStore xs = MkStore FO xs

indices :: (Bounded (Fin (Nadd N3 k)),Strengthenable Fin k) => Fin (Nadd N3 k) -> Vect N3 (Fin (Nadd N3 k))
indices x = down x :> x :> up x :> Nil
