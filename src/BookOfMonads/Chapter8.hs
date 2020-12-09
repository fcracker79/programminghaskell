module BookOfMonads.Chapter8 where

import Data.STRef
import Control.Monad.ST

sumall :: Int -> Int
sumall n = if n <= 0 then 0 else runST $ do
    a <- newSTRef 0
    modifySTRef a (+n)
    modifySTRef a (+ (sumall (n - 1)))
    readSTRef a


fibonacci :: Int -> Int
fibonacci n = if n < 2 then 1 else runST $ do
    return ((fibonacci (n - 1)) + (fibonacci (n - 2)))
