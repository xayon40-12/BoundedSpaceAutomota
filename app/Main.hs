module Main where

import System.Environment
import Fin
import GHC.Exts (fromList)
import Data.Functor.Rep
import Nat
import Rule
import Store
import Vect

type Size = N100

toBool i = i /= 0

parse (a:_) = read a
parse _ = 30

main :: IO ()
main = do
    args <- getArgs
    let rule = parse args
    runSimulation rule init
  where
    start :: Vect Size Bool
    start = tabulate (==FO)
    init :: Store (Vect Size) (Fin Size) Bool
    init = initialStore start
