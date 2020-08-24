module Chapter12 where
import Control.Monad

class MyFunctor f where
    myfmap :: (a -> b) -> f a -> f b


class MyApplicative f where
    mypure :: a -> f a
    (<**>) :: f (a -> b) -> f a -> f b

-- This is in the prelude, but if I do the same, as follows...
-- class MyFunctor2 (f :: * -> *) where
--    myfmap2 :: (a -> b) -> f a -> f b
-- I get:
--    Illegal kind signature: ‘* -> *’
--      Perhaps you intended to use KindSignatures
--    In the declaration for class ‘MyFunctor2’
-- BUT
--  Prelude> :info MyFunctor
--  class MyFunctor (f :: * -> *) where
--    myfmap :: (a -> b) -> f a -> f b
--    {-# MINIMAL myfmap #-}
--          -- Defined at <interactive>:3:1


instance MyFunctor IO where
    myfmap f o = do {x <- o; return (f x)}

instance MyApplicative IO where
    mypure = return
    (<**>) fio aio = do {f <- fio; a <- aio; return (f a)}

-- Thanks to Haskell prelude! (->) r is the type of all functions that takes an r as an argument.
instance MyFunctor ((->) r) where
    myfmap = (.)

g :: Int -> String
g = show

f :: Int -> Int
f = (+ 1)

f_mapped = myfmap g f

myGetChars :: Int -> IO String
myGetChars 0 = return []
-- myGetChars n = ((<**>) ((<**>) (mypure (:)) getChar) (myGetChars (n - 1)))
myGetChars n = mypure (:) <**> getChar <**> myGetChars (n - 1)