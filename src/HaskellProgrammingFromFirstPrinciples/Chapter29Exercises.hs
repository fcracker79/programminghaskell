module HaskellProgrammingFromFirstPrinciples.Chapter29Exercises where


import Criterion.Main
newtype DList a = DL { unDL :: [a] -> [a] }

empty :: DList a
empty = DL $ const []


singleton :: a -> DList a
singleton = DL . (:)

toList :: DList a -> [a]
toList = flip unDL []

infixr `cons`
cons :: a -> DList a -> DList a
cons x xs = DL ((x:) . unDL xs)

infixl `snoc`
snoc :: DList a -> a -> DList a
snoc xs x = DL $ \x0 -> unDL xs x0 ++ [x] 

append :: DList a -> DList a -> DList a
append a b = DL $ \x -> unDL a x ++ unDL b x 

instance Show a => Show (DList a) where
    show = show . toList

schlemiel :: Int -> [Int]
schlemiel i = go i []
  where go 0 xs = xs
        go n xs = go (n-1) ([n] ++ xs)

constructDlist :: Int -> [Int]
constructDlist i = toList $ go i empty
  where go 0 xs = xs
        go n xs = go (n-1) (singleton n `append` xs)

constructDlistUsingCons :: Int -> [Int]
constructDlistUsingCons i = toList $ go i empty
  where go 0 xs = xs
        go n xs = go (n-1) (cons n xs)


main :: Int -> IO ()
main n = defaultMain
  [ 
    bench "concat list" $ whnf schlemiel n, 
    bench "concat dlist" $ whnf constructDlist n,
    bench "concat dlist with cons" $ whnf constructDlistUsingCons n
  ]
