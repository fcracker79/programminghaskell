module Chapter7 where

mySum :: Num a => [a] -> a
mySum = foldr (+) 0

myProd :: Num a => [a] -> a
myProd = foldr (*) 1

myLength :: Num b => [a] -> b
myLength = foldr (\x -> (+ 1)) 0

myAll f x = foldr (&&) True $ map f x

myTakeWhile _ [] = []
myTakeWhile f (x:xs) | f x = x : takeWhile f xs
                   | otherwise = []


myDropWhile _ [] = []
myDropWhile f (x:xs) | f x = dropWhile f xs
                     | otherwise = x : xs

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x' -> \y' -> [f x'] ++ y') []
myFilter f = foldr (\x' -> \y' -> if (f x') then x' : y' else y') []

dec2int :: Num a => [a] -> a
dec2int = foldr (\x' -> \y' -> x' + 10 * y') 0

myCurry :: ((a, b) -> c) -> (a -> b -> c)
myCurry f = \x -> \y -> f (x, y)

myUncurry :: (a -> b -> c) -> ((a, b) -> c)
myUncurry f = \(x, y) -> f x y

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

int2bin x = unfold (== 0) (`mod` 2) (`div` 2) x

chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

chop8unfold = unfold (\x -> length x == 0) (take 8) (drop 8)

mapunfold :: (a->b) -> [a] -> [b]
mapunfold f = unfold (\x' -> (length x') == 0) (\x' -> f (x' !! 0)) (drop 1)

iterateunfold :: (a -> a) -> a -> [a]
iterateunfold f x = [x] ++ (unfold (\x -> False) f f x)

altmap :: (a->b) -> (a->b) -> [a] -> [b]
altmap f1 f2 = altmap1 f1 f2

altmap1 :: (a->b) -> (a->b) -> [a] -> [b]
altmap1 _ _ [] = []
altmap1 f1 f2 (x:xs) = [f1 x] ++ altmap2 f1 f2 xs

altmap2 :: (a->b) -> (a->b) -> [a] -> [b]
altmap2 _ _ [] = []
altmap2 f1 f2 (x:xs) = [f2 x] ++ altmap1 f1 f2 xs

luhnaltmap :: Integral a => [a] -> Bool
luhnaltmap x = sum (altmap id (\x' -> if (x' * 2) > 9 then x' * 2 - 9 else x' * 2) (reverse x)) `mod` 10 == 0
