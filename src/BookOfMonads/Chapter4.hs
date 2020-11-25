module BookOfMonads.Chapter4 where

import qualified BookOfMonads.Chapter1 as Ch1
import qualified Text.Read as TR
import Control.Monad.Loops

getChars = do
    x <- getChar
    if x == '\n' then
        return []
    else
        do
            xs <- getChars
            return (x:xs)


getCharsUntilNumber :: Int -> IO String

getCharsUntilNumber n = iterateWhile
    (\x -> case fmap ((>=) n) (TR.readMaybe x::Maybe Int) of
           Nothing -> True
           Just x -> x) getChars