module Fin where

import Nat

data Fin (n :: N) where
    FO :: Fin (S n) -- One element
    FS :: Fin n -> Fin (S n) -- n + 1 element
instance Show (Fin Z) where
    show = undefined -- unreachable
instance (Enum (Fin (S n))) => Show (Fin (S n)) where
    show n = show (fromEnum n)
deriving instance Eq (Fin n)
deriving instance Ord (Fin n)

instance Enum (Fin (S Z)) where
    toEnum :: Int -> (Fin (S Z))
    toEnum 1 = FO
    toEnum i = error "Index out of bound."
    fromEnum :: (Fin (S Z)) -> Int
    fromEnum FO = 1
    fromEnum (FS _) = undefined -- Not rechable as Fin (S Z) is inhabited only by FO

instance (Enum (Fin (S n))) => Enum (Fin (S (S n))) where
    toEnum 1 = FO
    toEnum i = FS . toEnum $ i-1
    fromEnum FO = 1
    fromEnum (FS n) = 1 + fromEnum n

type Bf k = Bounded (Fin k)
instance Bounded (Fin (S Z)) where
    minBound = FO
    maxBound = FO
instance (Bounded (Fin (S n))) => Bounded (Fin (S (S n))) where
    minBound = FO
    maxBound = FS maxBound

weaken :: Fin k -> Fin (S k)
weaken FO = FO
weaken (FS n) = FS (weaken n)

class Strengthenable a n where
    strengthen :: a (S n) -> Either (a (S n)) (a n)

type Sf = Strengthenable Fin
instance Strengthenable Fin Z where
    strengthen FO = Left FO
    strengthen _ = undefined -- unreachable

instance (Sf k) => Strengthenable Fin (S k) where
    strengthen FO = Right FO
    strengthen (FS n) = case strengthen n of
                    Left x -> Left $ FS x
                    Right x -> Right $ FS x

down :: (Bf (S k)) => Fin (S k) -> Fin (S k)
down FO = maxBound
down (FS k) = weaken k

up :: (Sf k) => Fin (S k) -> Fin (S k)
up = either (const FO) FS . strengthen
