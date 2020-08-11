{-# LANGUAGE FlexibleInstances #-}

module Chapter8 where

type Position = (Double, Double)
distance :: Position -> Double
distance (x,y) = sqrt(x * x + y * y)

data Movement = North | South | East | West
onemove :: Movement -> Position -> Position
onemove North (x,y) = (x, y - 1)
onemove South (x,y) = (x, y + 1)
onemove East (x,y) = (x + 1, y)
onemove West (x,y) = (x - 1, y)

move :: [Movement] -> Position -> Position
move [] p = p
move (x:xs) p = move xs (onemove x p)

data Nat = Zero | Succ Nat  deriving Show

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

addnat :: Nat -> Nat -> Nat
addnat x y = int2nat ( nat2int x + nat2int y)


data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

occurs :: Eq a => Tree a -> a -> Bool
occurs (Leaf x) y = x == y
occurs (Node l x r) y = x == y || (occurs l y) || (occurs r y)

leaves :: Tree a -> [Tree a]
leaves (Leaf x) = [Leaf x]
leaves (Node l x r) = (leaves l) ++ (leaves r)

class HelloWorld a where
    hello :: a -> String

instance HelloWorld Bool where
    hello x = "hello bool " ++ show x

instance HelloWorld Int where
    hello x = "hello int " ++ show x

-- This requires Flexible Instances as String is not a primitive type
instance HelloWorld String where
    hello x = "hello string " ++ x
