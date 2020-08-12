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

data Nat = Zero | Succ Nat deriving Show

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


occursOrdered :: (Ord a, Eq a) => Tree a -> a -> Bool
occursOrdered (Leaf x) y = x == y
occursOrdered (Node l x r) y | x == y = True
                      | x < y = occurs r y
                      | otherwise = occurs l y


data Tree2 a = Leaf2 a | Node2 (Tree2 a) (Tree2 a) deriving Show

leaves2 :: Tree2 a -> Int
leaves2 (Leaf2 _) = 1
leaves2 (Node2 l r) = (leaves2 l) + (leaves2 r)

balanced :: Tree2 a -> Bool
balanced (Leaf2 _) = True
balanced (Node2 l r) = leaves_left - leaves_right <= 1 && leaves_left - leaves_right >= -1 && balanced l && balanced r
             where leaves_left = leaves2 l
                   leaves_right = leaves2 r


leaves :: Tree a -> [Tree a]
leaves (Leaf x) = [Leaf x]
leaves (Node l x r) = (leaves l) ++ (leaves r)

balanced_tree :: [a] -> Tree2 a
balanced_tree x
    | length x == 1 = Leaf2 (x !! 0)
    | otherwise = Node2 left_node right_node
   where first_half_size = length x `div` 2
         left_node = balanced_tree (take first_half_size x)
         right_node = balanced_tree (drop first_half_size x)

class HelloWorld a where
    hello :: a -> String

instance HelloWorld Bool where
    hello x = "hello bool " ++ show x

instance HelloWorld Int where
    hello x = "hello int " ++ show x

-- This requires Flexible Instances as String is not a primitive type
instance HelloWorld String where
    hello x = "hello string " ++ x

mulnat :: Nat -> Nat -> Nat
mulnat _ Zero = Zero
mulnat x (Succ y) = addnat (mulnat x y) x

data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a

folde f g (Val x) = f x
folde f g (Add x1 x2) = g (folde f g x1) (folde f g x2)

eval :: Expr -> Int
eval = folde id (\x1 x2 -> x1 + x2)

size :: Expr -> Int
size = folde (\_ -> 1) (\x1 x2 -> x1 + x2)


data Maybe2 a = Nothing2 | Just2 a
instance Eq a => Eq (Maybe2 a) where
    (==) (Just2 x) (Just2 y) = x == y
    (==) Nothing2 Nothing2 = True
    (==) _ _ = False
