module HaskellProgrammingFromFirstPrinciples.Chapter29Exercises where


import Criterion.Main
import Data.Maybe (fromMaybe)
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


data Queue a = Queue { enqueue :: [a], dequeue :: [a] } deriving (Eq, Show)

push :: a -> Queue a -> Queue a
push a (Queue _enqueue _dequeue) = Queue (a:_enqueue) _dequeue

pop :: Queue a -> Maybe (a, Queue a)
pop q = case q of
  Queue [] [] -> Nothing
  Queue q (x: xs) -> Just (x, Queue q xs)
  Queue q [] -> Just (x, Queue [] xs) where (x:xs) = reverse q


testQueue :: Queue String -> Int -> Queue String
testQueue x 0 = x
testQueue x n = testQueue ((snd . fromMaybe ("x", Queue [] []) . pop . push "x") x) (n - 1)

testList :: [String] -> Int -> [String]
testList x 0 = x
testList x n = testList xa (n - 1)
    where (_:xa) = reverse ("1":x)

initialArray :: [String]
initialArray = fmap show [0..10000]

main :: Int -> IO ()
main n = defaultMain
  [ 
    bench "concat list" $ whnf schlemiel n, 
    bench "concat dlist" $ whnf constructDlist n,
    bench "concat dlist with cons" $ whnf constructDlistUsingCons n,
    bench "queue" $ whnf (testQueue (Queue initialArray [])) 1000,
    bench "list" $ whnf (testList initialArray) 1000
  ]
