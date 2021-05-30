{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
module HaskellInDepth.CH4.Relevants where

import qualified Universum as U
import Control.Lens (Profunctor(lmap, rmap))
import Data.Monoid (Sum(..))
import Data.Semigroup (getSum)
import Control.Comonad (extend)


myNonEmptyList :: U.NonEmpty [Char]
myNonEmptyList = "MyHead" U.:| ["My", "Tail"]
myList :: [[Char]]
myList = ["MyHead", "My", "Tail"]


-- head [] would fail
myHead :: [Char]
myHead = U.head myNonEmptyList
mySafeHead :: Maybe [Char]
mySafeHead = U.safeHead myNonEmptyList
mySafeHeadUsingBareList :: Maybe [Char]
mySafeHeadUsingBareList = U.safeHead myList

-- tail [] would fail7
myTail :: [[Char]]
myTail = U.tail myNonEmptyList

-- init [] would fail
myInit :: [[Char]]
myInit = U.init myNonEmptyList

-- map is fmap
{-
GHCi> import qualified Universum as U
GHCi> :info U.map
Universum.map :: Functor f => (a -> b) -> f a -> f b
-}

myPutStrLn :: IO ()
{- 
  This does not compile as U.putStrLn is polymorphic and we have OverloadedStrings enabled.

  myPutStrLn = U.putStrLn "hello"
  
  U.putStrLn takes a U.Print as input, which can be a String, a Text or a ByteString.
-}
myPutStrLn = U.putTextLn "hello"

oldPutStrLn :: IO ()
oldPutStrLn = putStrLn "hello"


-- U.show != show
-- TODO verify which issues to face when implementing Show

data MyDino = MyDino

instance Show MyDino where
    show _ = "This is Dino"


myShowDino :: [Char]
myShowDino = U.show MyDino


-- New features
mySplitHeadTail :: Maybe ([Char], [[Char]])
mySplitHeadTail = U.uncons myList


-- Not strictly related to the Chapter 4 of the book

newtype Whatever = Whatever Int
data Dino a = forall b . Monad b => Dino (b a)
data Dino2 x = forall a . Monad a => Dino2 (a x)


x :: Dino2 Int
x = Dino2 (Just (1::Int))
x1 :: Dino2 Int
x1 = Dino2 [2::Int]
x2 :: Dino2 Int
x2 = Dino2 (Right 3)
--x3 = Dino2 (Whatever 4)
x4 :: Dino2 Int
x4 = Dino2 (Right (3::Int))
x5 :: Dino2 Int
x5 = Dino2 (Right 3)

-- Nice functor functions

niceFunctor1 :: Maybe [Char]
niceFunctor1 = Nothing U.$> "hello"

niceFunctor2 :: Maybe [Char]
niceFunctor2 = "hello" U.<$ Nothing

-- Profunctors
fPro :: Int -> String 
fPro = show

flmap :: () -> Int
flmap = const 32

lfPro :: () -> String
lfPro = lmap flmap fPro

frmap :: String -> Int
frmap x = read x :: Int

rfPro :: () -> Int
rfPro = rmap frmap lfPro

{- 
Comonads

  extract :: w a -> a
  extract . fmap f = f . extract   -- (1)

  Given 
    f :: a -> b
    fmap :: (a -> b) -> w a -> w b
  
  we have:
    fmap f = wa -> wb
  
  So `extract` law (1) may be decomposed into the following:
  (w b -> b) . (wa -> wb) = (a -> b) . (w a -> a)

  The law (1) applies the `extract` function after the `fmap` transformation on the left side, 
  whereas does that before that transformation on the right side.

  extend :: (w a -> b) -> w a -> w b

  We can say that
  
  extend f = fmap f . duplicate

  Given:
    duplicate :: w a -> w (w a)
    f :: w a -> b
    fmap f :: w (w a) -> w b
  we have:
    fmap f (w (w a))
    (w (w a) -> w b) (w (w a))
    w b

  Just to understand the sense of that
  
  Using Tuple (a, b) for our [co]monad, we have:
  join :: Monoid c => (c, (c, a)) -> (c, a)  -- From Monad
  duplicate :: (c,a) -> (c, (c, a))          -- From Comonad
  
  Using (a -> b) for our [co]monad, we have:
  join :: (r -> r -> a) -> r -> a
  duplicate :: Monoid r => (r -> a) -> r -> r -> a

  There is a simmetry between Monad.join and Comonad.duplicate.

  The same applies for the bind operator as follows

  (>>=) :: m a -> (a -> m b) -> m b
  (=>>) :: m a -> (m a -> b) -> m b
  
  (=>>) = flip extend

  Instances
  Reader is a monad and a CoWriter (a.k.a Tracer) comonad
  Tuple is a writer as monad and a CoReader (a.k.a Env) comonad
-}

{-
extend :: (w a -> b) -> w a -> w b


((r -> a) -> b) -> (r -> a) -> (r -> b)


import Data.Monoid
import Control.Comonad
:{
data Dino = Dino

f :: String -> Int
f = read

g :: String -> Dino
g = extend (const Dino) f
:}



:{
f :: Sum Int -> Int
f = getSum
:}

:{
ff :: Int -> String
ff = Sum . read
:}


g :: Sum Int -> String -> Int
g = extend (\rf -> rf . ff) f



instance             Monad   ((->) r) 
instance Monoid m => Comonad ((->) m)


-}

-- Here is an example of comonad usage with reader


f :: Sum Int -> Int
f = getSum

ff :: String -> Sum Int
ff = Sum . read

g :: Sum Int -> String -> Int
g = extend ( . ff) f

newtype Dino3 = Dino3 Int
g' :: Sum Int -> Dino3
g' = extend (const (Dino3 (42::Int))) f

g'' :: Sum Int -> String
g'' = extend ff f
  where ff f' = show $ f (Sum 42)


g''' :: Sum Int -> b
g''' = extend ff f
  where 
    ff :: (Sum Int -> Int) -> b
    ff = undefined

g'''m :: Sum Int -> b
g'''m = f >>= ff
  where 
    ff :: Int -> Sum Int -> b
    ff = undefined

f2 :: (Int, String) -> Dino3
f2 = undefined
