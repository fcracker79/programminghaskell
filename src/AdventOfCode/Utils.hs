module AdventOfCode.Utils(
    qsort,
    binarySearch,
    split
) where


qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller_ones ++ [x] ++ qsort greater_ones
                where
                    smaller_ones = [a | a <- xs, a <= x]
                    greater_ones = [a | a <- xs, a > x]


binarySearch :: [Int] -> Int -> Maybe Int
binarySearch [] _ = Nothing
binarySearch a x = recBinarySearch a x 0 $ length a - 1

recBinarySearch :: [Int] -> Int -> Int -> Int -> Maybe Int
recBinarySearch a x p0 p1 
    | v0 == x = Just v0
    | v1 == x = Just v1
    | p0 == p1 = Nothing 
    | v0 < x = recBinarySearch a x nextP p1
    | v0 > x = recBinarySearch a x p0 nextP
    where v0 = a !! p0
          v1 = a !! p1
          nextP = quot p0 p1


split   :: String -> Char -> [String]
split s c =  case dropWhile (== c) s of
                    "" -> []
                    s' -> w : split s'' c
                            where (w, s'') = break (== c) s'
