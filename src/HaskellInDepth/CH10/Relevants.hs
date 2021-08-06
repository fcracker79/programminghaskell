module HaskellInDepth.CH10.Relevants where

import Criterion.Main

primeNumber :: Integer
primeNumber = 16183

isPrime :: Integer -> Bool
isPrime n = all notDividedBy [2 .. n-1]
    where notDividedBy m = n `mod` m /= 0

isPrimeUnfolded :: Integer -> Bool
isPrimeUnfolded n = go 2
    where
        go x = (x > n-1) ||  ((n `mod` x /= 0) && go (x + 1))

isPrimeUnfolded2 :: Integer -> Bool
isPrimeUnfolded2 n = go 2
  where
    go x = case x > n-1 of
             True -> True
             False -> case n `mod` x /= 0 of
                        True -> go (x+1)
                        False -> False


isPrimeRewritten :: Integer -> Bool
isPrimeRewritten n = all notDividedBy [2 .. n `div` 2]
    where notDividedBy m = n `mod` m /= 0

main :: IO ()
main = defaultMain [
        bgroup "single" [
            bench "isPrime (declarative)" $ whnf isPrime primeNumber
            , bench "isPrime (unfolded)" $ whnf isPrimeUnfolded primeNumber
            , bench "isPrime (unfoldedOrig)" $ whnf isPrimeUnfolded2 primeNumber
            , bench "isPrime (unfolded rewritten)" $ whnf isPrimeRewritten primeNumber
        ], 
        bgroup "many" [
            bench "isPrime (declarative)" $ nf  (fmap isPrimeRewritten) [0..1000]
            , bench "isPrime many" $ nf  (fmap isPrimeRewritten) [0..1000]
        ],
        bgroup "env" $ fmap (\i -> env (pure (i * 10)) (bench "dino" . nf isPrimeRewritten)) [0..10]
    ]
