module Rule where

import Vect
import Fin
import Nat
import Store
import Control.Comonad
import Control.Monad
import Data.Functor.Rep

type Na3 k = Nadd N3 k
type V3 k = Vect (Na3 k)
type F3 k = Fin (Na3 k)
type S3 k = Store (V3 k) (F3 k)
type BSR k = (Rv k,Bf (S k),Sf k)

initialStore :: V3 k Bool -> S3 k Bool
initialStore xs = MkStore FO xs

indices :: (Bf (Na3 k),Sf k) => F3 k -> Vect N3 (F3 k)
indices x = down x :> x :> up x :> Nil

neighbors :: (BSR k) => S3 k Bool -> Vect N3 Bool
neighbors = experiment indices

isAlive :: (BSR k) => S3 k Bool -> Bool
isAlive s = case neighbors s of
    (False :> False :> False :> Nil) -> False
    (True :> False :> False :> Nil) -> False
    (True :> True :> True :> Nil) -> False
    _ -> True

nextGen :: (BSR k) => S3 k Bool -> S3 k Bool
nextGen = extend isAlive

universe :: (Rv k) => Vect k (Fin k)
universe = tabulate id

boolToString :: Bool -> String
boolToString False = "0"
boolToString True = "1"
