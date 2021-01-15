module AdventOfCode.Y2020.AOC10(joltDifferences) where 


import AdventOfCode.Utils(qsort)
foldFunction :: (Int, Int, Int, Int) -> Int -> (Int, Int, Int, Int)
foldFunction (count1, count2, count3, oldValue) newValue
    | newValue - oldValue == 1 = (count1 + 1, count2, count3, newValue)
    | newValue - oldValue == 2 = (count1, count2 + 1, count3, newValue)
    | newValue - oldValue == 3 = (count1, count2, count3 + 1, newValue)
    | otherwise = (count1, count2, count3, newValue)

joltDifferences :: [Int] -> Int
joltDifferences l = count1 * count3
    where 
        (count1, _, count3, _) = foldl foldFunction (0, 0, 0, 0) finalList
        finalList = sortedList ++ [3 + maximum sortedList]
        sortedList = qsort l
