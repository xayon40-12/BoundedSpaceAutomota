module Nat where

data N = Z | S N deriving (Show, Eq, Ord)

instance Enum N where
  toEnum 0 = Z
  toEnum i = S . toEnum $ i -1
  fromEnum Z = 0
  fromEnum (S n) = 1 + fromEnum n

type family Nadd a b where
  Nadd Z n = n
  Nadd (S m) n = S (Nadd m n)

type family Nmul a b where
  Nmul Z _ = Z
  Nmul (S m) n = Nadd n (Nmul m n)

{- ORMOLU_DISABLE -}
type N0 = Z
type N1 = S N0
type N2 = S N1
type N3 = S N2
type N4 = S N3
type N5 = S N4
type N6 = S N5
type N7 = S N6
type N8 = S N7
type N9 = S N8
type N10 = Nadd N5 N5
type N100 = Nmul N10 N10
type N1000 = Nmul N10 N100
type N10000 = Nmul N10 N1000
{- ORMOLU_ENABLE -}
