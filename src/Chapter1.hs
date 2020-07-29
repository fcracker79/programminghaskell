module Chapter1 where
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller_ones ++ [x] ++ qsort greater_ones
               where
                   smaller_ones = [a | a <- xs, a <= x]
                   greater_ones = [a | a <- xs, a > x]
