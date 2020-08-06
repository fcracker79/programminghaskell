module Chapter5 where

factors :: Integral a => a -> [a]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Integral a => a -> Bool
prime n = factors n == [1, n]

firstAndLast :: [a] -> [(a, a)]
firstAndLast xs = zip xs (reverse xs)


position :: (Num i, Enum i, Eq a) => [a] -> a -> [i]
position xs x = [i | (i, x') <- zip [0..] xs, x' == x]

grid :: Integral a => a -> a-> [(a, a)]
grid x y = [(x', y') | x' <- [0..x], y' <- [0..y]]

square :: Integral a => a -> [(a, a)]
square x = [(x', y') | (x', y') <- grid x x, x' /= y']

replicate2 :: Integral i => i -> a -> [a]
replicate2 i a = [a | _ <- [1..i]]

pyths x = [
    (x', y', z') |
    x' <- [1..x], y' <- [1..x], z' <- [1..x],
    x' * x' + y' * y' == z' * z']
