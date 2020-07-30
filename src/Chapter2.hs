module Chapter2 where

factorial :: (Enum a, Num a) => a -> a
factorial x = product [1..x]
