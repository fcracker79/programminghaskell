{-# LANGUAGE OverloadedStrings #-}
module HaskellInDepth.CH4.Relevants where

import qualified Universum as U


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