module Main where

import Rule
import Vect
import Nat
import Fin
import Store
import GHC.Exts (fromList)

type Size = Nadd N10 N4

main :: IO ()
main = runSimulation init
    where start :: Vect Size Bool
          start = fromList $ map (\i -> if i == 0 then False else True) [0,0,0,1,0,0,1,1,0,1,1,1,1,1]
          init :: Store (Vect Size) (Fin Size) Bool
          init = initialStore start
