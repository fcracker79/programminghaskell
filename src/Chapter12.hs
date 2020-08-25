module Chapter12 where
import Control.Monad

class MyFunctor f where
    myfmap :: (a -> b) -> f a -> f b


class MyApplicative f where
    mypure :: a -> f a
    (<**>) :: f (a -> b) -> f a -> f b

class MyApplicative m => MyMonad m where
    myreturn :: a -> m a
    (>>==) :: m a -> (a -> m b) -> m b
    myreturn = mypure

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


instance MyApplicative ((->) r) where
    mypure x = \r -> x
    (<**>) g x = \r -> (g r) (x r)


-- In this example r is Int and f is a function that takes Int
-- a is IO String
-- b is IO Int
-- So, given (Int -> IO String), I want a function (Int -> IO Int).
-- In order to implement it, we use an applicative that applies to a function
-- that converts a into b, that is, parses the String and "returns" the Int
myGetString :: Int -> IO String
myGetString = myGetChars

myIOParse :: (IO String) -> (IO Int)
myIOParse x = do {v <- x; return (read v :: Int)}

myGetInt :: Int -> IO Int
myGetInt = (mypure myIOParse) <**> myGetString


myMapM :: (MyMonad m) => (a -> m b) -> [a] -> m [b]
myMapM _ [] = mypure []
myMapM f (x:xs) = f x >>== \y -> (myMapM f xs) >>== \ys -> mypure (y:ys)

-- "do" constructor just works with a real monad, not with MyMonad.
myMapMDo :: (Monad m) => (a -> m b) -> [a] -> m [b]
myMapMDo _ [] = pure []
myMapMDo f (x:xs) = do
              y <- f x
              ys <- myMapMDo f xs
              return (y:ys)
