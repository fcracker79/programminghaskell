{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}
module HaskellInDepth.CH11.Types101_3 where


data a + b = Inl a | Inr b deriving Show
data a * b = a :*: b deriving Show

infixl 6 +
infixl 7 *

val1 :: Int + Bool * Bool
val1 = Inl 0
val2 :: a + (Bool, Bool)
val2 = Inr (True, False)


valEither :: Either Int (Bool, Bool)
valEither = Left 0
valEither2 :: Either Int (Bool, Bool)
valEither2 = Right (True, False)

-- (a + a * a) + ((a * a) * a)
type Point a = a + a * a + a * a * a 


point1D :: a -> Point a
point1D x = Inl $ Inl x
point2D :: a -> a -> Point a
point2D x y = Inl $ Inr (x :*: y)
point3D :: a -> a -> a -> Point a
point3D x y z = Inr $ (x :*: y) :*: z

main :: IO ()
main = do
    putStr "Point 1D: "
    print $ point1D 5

    putStr "Point 2D: "
    print $ point2D 10 22

    putStr "Point 3D: "
    print $ point3D 40 13 27
