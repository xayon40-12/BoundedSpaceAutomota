module Main where

import Fin
import GHC.Exts (fromList)
import Nat
import Rule
import Store
import Vect

type Size = N100

toBool i = i /= 0

main :: IO ()
main = runSimulation init
  where
    start :: Vect Size Bool
    start = fromList $ map toBool ([0 | _ <- [1 .. 99]] ++ [1])
    init :: Store (Vect Size) (Fin Size) Bool
    init = initialStore start
