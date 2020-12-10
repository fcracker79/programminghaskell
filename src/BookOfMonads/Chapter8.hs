module BookOfMonads.Chapter8 where

import Data.STRef
import Control.Monad.ST
import Control.Exception

sumall :: Int -> Int
sumall n = if n <= 0 then 0 else runST $ do
    a <- newSTRef 0
    modifySTRef a (+n)
    modifySTRef a (+ (sumall (n - 1)))
    readSTRef a


fibonacci :: Int -> Int
fibonacci n = if n < 2 then 1 else runST $ do
    return ((fibonacci (n - 1)) + (fibonacci (n - 2)))


data MyException = AnException | AnotherException deriving(Show)
instance Exception MyException

throwSomeExceptions :: Int -> IO Int
throwSomeExceptions x = case x of
         0 -> throw AnException
         1 -> throw AnotherException
         otherwise -> return $ x + 1

catchMyException :: MyException -> IO Int
catchMyException e = do
    case e of
        AnException -> return 1000
        AnotherException -> return 2000

catchSomeException :: Int -> IO Int
catchSomeException n = throwSomeExceptions n `catches` [Handler catchMyException]
