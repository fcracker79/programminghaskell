module Chapter4 where

evenOdd :: Integral a => [a] -> ([a], [a])

-- This was not what the book's exercise requested, but it is pretty cool, as it summarizes
-- some of the topics of the chapter: cons and guards
evenOdd [] = ([], [])
evenOdd (x:xs) | x `mod` 2 == 0 = (x : all_the_other_even, all_the_other_odd)
             | otherwise = (all_the_other_even, x : all_the_other_odd)
    where
        (all_the_other_even, all_the_other_odd) = evenOdd xs


halve :: [a] -> ([a], [a])
halve x = (take n x, drop n x) where n = length x `div` 2

third1 x = head (tail(tail x))
third2 :: [a] -> a
third2 x = x !! 2

third3 :: [a] -> a
third3 (_:_:x:_) = x

safetailConditionalExpression :: [a] -> [a]
safetailConditionalExpression x = if null x then x else tail x

safetailGuardedEquation x | null x = x
                          | otherwise = tail x

safetailPatternMatching [] = []
-- more elegant than safetailPatternMatching x = tail x
safetailPatternMatching (x:xs) = xs

mult = \x -> (\y -> (\z -> x * y * z))

luhnDouble x | dx > 9 = dx - 9
            | otherwise = dx
            where dx = x * 2

luhn :: Integral a => [a] -> Bool
luhn x = if finalSum `mod` 10 == 0 then True else False
    where finalSum = (sum oddElements) + sum (map luhnDouble evenElements)
          (oddElements, evenElements) = evenOddIndex $ reverse x

evenOddIndex :: [a] -> ([a], [a])
evenOddIndex x = evenOddIndex0 x 0

evenOddIndex0 :: Integral x => [a] -> x -> ([a], [a])

evenOddIndex0 [] _ = ([], [])
evenOddIndex0 (x:xs) 0 = (x: no, ne) where (no, ne) = evenOddIndex0 xs 1
evenOddIndex0 (x:xs) 1 = (no, x: ne) where (no, ne) = evenOddIndex0 xs 0
