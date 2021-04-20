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