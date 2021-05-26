{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
module HaskellInDepth.CH4.Relevants where

import qualified Universum as U
import Control.Lens (Profunctor(lmap, rmap))


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
