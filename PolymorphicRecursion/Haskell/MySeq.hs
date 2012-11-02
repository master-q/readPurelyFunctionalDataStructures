module MySeq (MySeq(..), sizeS) where

data MySeq a = MSNil | MSCons (a, MySeq (a,a))

sizeS :: MySeq a -> Int
sizeS MSNil = 0
sizeS (MSCons (_, ps)) = 1 + 2 * sizeS ps
