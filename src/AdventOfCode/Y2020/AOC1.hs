module AdventOfCode.Y2020.AOC1 where

import Control.Monad(guard)
import Data.Maybe(isJust)
import AdventOfCode.Utils(qsort, binarySearch)


findSumUp2020 :: [Int] -> Maybe Int
findSumUp2020 x = 
    case result of 
        [] -> Nothing
        (x0:_) -> Just x0
    where result = do
                   a <- xs
                   let b = binarySearch xs (2020 - a)
                   guard (isJust b)
                   let Just jb = b
                   return $ a * jb
          xs = qsort x
