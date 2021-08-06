{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
module HaskellInDepth.CH9.Relevants where

import RIO
import System.Environment
import System.TimeIt

y True x = ""

y' True = \x -> ""


{-
OUTPUT:
"Evaluating y"
0
"Evaluating y'"
*** Exception: /home/mirko/dev/haskell/programminghaskell/src/HaskellInDepth/CH9/Relevants.hs:5:1-18: Non-exhaustive patterns in function y'
-}
main :: IO ()
main = do
    let (# a, b #) = sumProd (1::Int) (2::Int)
    print $ "a: " ++ show a ++ ", b: " ++ show b
    let g = y False
    let g' = y' False
    -- Forcing evaluation
    print "Evaluating y"
    print $ seq g a
    print "Evaluating y'"
    print $ seq g' b
    print "Unreachable code"

{-
GHCi> :set +s
GHCi> M.sumNNoThunk  1000000
500000500000
(1.68 secs, 345,146,192 bytes)
GHCi> M.sumN  1000000
500000500000
(1.95 secs, 378,766,160 bytes)
GHCi> M.sumNDeepSeq   1000000
500000500000
(2.27 secs, 473,146,304 bytes)
-}
sumN :: Int -> Int
sumN n = go 0 n
    where
        go acc 0 = acc
        go acc n = go (acc+n) (n-1)

sumNNoThunk :: Int -> Int
sumNNoThunk n = go 0 n
    where
        go acc 0 = acc
        go !acc n = go (acc+n) (n-1)

sumNDeepSeq :: (Eq t, Num t, NFData t) => t -> t
sumNDeepSeq n = go 0 n
    where
        go acc 0 = acc
        go acc n = deepseq acc $ go (acc+n) (n-1)


data Shape = Rectangle {-# UNPACK #-} !Int {-# UNPACK #-} !Int | Circle {-# UNPACK #-} !Int

sumProd :: Num a => a -> a -> (# a, a #)
sumProd a b = (# a + b, a*b #)



isPrime :: Integer -> Bool
isPrime n = all notDividedBy [2 .. n-1]
    where notDividedBy m = n `mod` m /= 0


mainPrime :: IO ()
mainPrime = getArgs >>= timeIt . print . isPrime . read . head