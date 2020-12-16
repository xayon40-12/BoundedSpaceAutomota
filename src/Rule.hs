module Rule where
    
import Control.Comonad
import Control.Concurrent
import Data.Foldable (traverse_)
import Data.Functor.Rep
import Data.Bits
import Fin
import GHC.Exts (toList)
import Nat
import Store
import Vect

type Na3 k = Nadd N3 k

type V3 k = Vect (Na3 k)

type F3 k = Fin (Na3 k)

type S3 k = Store (V3 k) (F3 k)

type BSR k = (Rv k, Bf (S k), Sf k)

initialStore :: V3 k Bool -> S3 k Bool
initialStore = MkStore FO

indices :: (Bf (Na3 k), Sf k) => F3 k -> Vect N3 (F3 k)
indices x = down x :> x :> up x :> Nil

neighbors :: (BSR k) => S3 k Bool -> Vect N3 Bool
neighbors = experiment indices

isAlive :: (BSR k) => Int ->  S3 k Bool -> Bool
isAlive rule s = case neighbors s of a :> b :> c :> Nil -> testBit rule (f a 4 + f b 2 + f c 1)
    where f x m = if x then m else 0

nextGen :: (BSR k) => Int -> S3 k Bool -> S3 k Bool
nextGen rule = extend (isAlive rule)

universe :: (Rv k) => Vect k (Fin k)
universe = tabulate id

boolToString :: Bool -> String
boolToString False = " "
boolToString True = "#"

printState :: (Lv k Bool) => V3 k Bool -> IO ()
printState xs = do
  traverse_ (putStr . boolToString) $ toList xs
  putStrLn ""

runSimulation :: forall k. (Lv k Bool, BSR k) => Int -> S3 k Bool -> IO ()
runSimulation rule s = do
  if and lcurr || all not lcurr
    then printState curr
    else
      printState curr
        >>= ( \_ -> do
                threadDelay 100000
                runSimulation rule (nextGen rule s)
            )
  where
    curr :: (Lv k Bool, BSR k) => V3 k Bool
    curr = experiment (const universe) s
    lcurr = toList curr
