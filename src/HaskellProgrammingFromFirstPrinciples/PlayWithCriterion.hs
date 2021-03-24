module HaskellProgrammingFromFirstPrinciples.PlayWithCriterion where


import Criterion.Main ( bench, whnf, defaultMain )
infixl 9 !?
(!?) :: (Ord t, Num t) => [a] -> t -> Maybe a
_      !? n | n < 0 = Nothing
[]     !? _ = Nothing
(x:_)  !? 0 = Just x
(_:xs) !? n = xs !? (n-1)

myList :: [Int]
myList = [1..9999]

main :: IO ()
main = defaultMain
    [ 
        bench "index list 9999" $ whnf (myList !!) 9998, 
        bench "index list maybe index 9999" $ whnf (myList !?) 9998
    ]


(!!!) :: (Ord t1, Foldable t2, Num t1) => t2 a -> t1 -> Maybe a
xs !!! n
    | n < 0 = Nothing
    | otherwise = foldr dino (const Nothing) xs n
                        where dino = \x r k -> case k of
                                        0 -> Just x
                                        _ -> r (k-1)