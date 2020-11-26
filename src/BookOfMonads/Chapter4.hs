module BookOfMonads.Chapter4 where

import qualified BookOfMonads.Chapter1 as Ch1
import qualified Text.Read as TR
import Control.Monad.Loops


getChars :: IO String
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

-- whileM :: Monad m => m Bool -> m a -> m [a]

printStuffWhile :: IO [String]
printStuffWhile = whileM
    (do
        print "Continue ?"
        x <-fmap ((==) 'y') getChar
        putStrLn ""
        return x
    )
    getChars


printStuffIterateWhile :: Int -> IO (Maybe Int)
printStuffIterateWhile n =
    iterateWhile
    (\x -> case x of
           Nothing -> True
           Just y -> y < n)
    $ fmap TR.readMaybe getChars
