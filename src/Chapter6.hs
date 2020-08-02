module Chapter6 where

firstAndLast :: [a] -> [(a, a)]
firstAndLast xs = zip xs (reverse xs)


position :: (Num i, Enum i, Eq a) => [a] -> a -> [i]
position xs x = [i | (i, x') <- zip [0..] xs, x' == x]
