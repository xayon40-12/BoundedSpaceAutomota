module Rule where

import Vect
import Fin
import Nat
import Store

initialStore :: Vect (Nadd N3 k) Bool -> Store (Vect (Nadd N3 k)) (Fin (Nadd N3 k)) Bool
initialStore xs = MkStore FO xs
