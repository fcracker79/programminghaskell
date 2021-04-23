module HaskellInDepth.CH1.Relevants where

import Data.Text
import Fmt ( (+|), (|+) )
f x@(x0:xs) = "All the elements are " +|x|+ ", the first element is " +|x0|+ ", the tail is " +|xs|+""


fmonad :: Monad m => m a -> (a -> m b) -> m b
fmonad = (>>=) 

fmonad2 :: Monad m => (a -> m b) -> m a -> m b
fmonad2 = (=<<)


readTextFile :: FilePath -> IO String
readTextFile = readFile


-- Nice idea on Enum
class (Eq a, Enum a, Bounded a) => CyclicEnum a where
    cpred :: a -> a
    cpred d
        | d == minBound = maxBound
        | otherwise = pred d
    csucc :: a -> a
    csucc d
        | d == maxBound = minBound
        | otherwise = succ d
