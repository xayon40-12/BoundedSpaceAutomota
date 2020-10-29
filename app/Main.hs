module Main where

import Rule
import Vect
import Nat
import Fin
import Store
import GHC.Exts (fromList)

type Size = N100

toBool i = if i == 0 then False else True

main :: IO ()
main = runSimulation init
    where start :: Vect Size Bool
          start = fromList $ map toBool ([0|_<-[1..99]] ++ [1])
          init :: Store (Vect Size) (Fin Size) Bool
          init = initialStore start
