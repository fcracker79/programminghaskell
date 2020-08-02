module Chapter5 where

factors :: Integral a => a -> [a]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Integral a => a -> Bool
prime n = factors n == [1, n]
