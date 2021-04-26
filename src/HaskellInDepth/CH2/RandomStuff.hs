module HaskellInDepth.CH2.RandomStuff where


import System.Random
    ( getStdGen, getStdRandom, uniform, uniformR, UniformRange(..), Uniform(..))
import System.Random.Internal (UniformRange(uniformRM))
import System.Random.Stateful (Uniform(uniformM))
import Control.Monad.IO.Class (MonadIO)


main :: IO ()
main = do
    m1 <- getStdRandom uniform :: IO Int
    m2 <- getStdRandom uniform :: IO Int
    print $ "m1:" ++ show m1
    print $ "m2:" ++ show m2
    m3 <- getStdRandom (uniformR (0, 100)) :: IO Int
    m4 <- getStdRandom (uniformR (0, 100)) :: IO Int
    print $ "m3:" ++ show m3
    print $ "m4:" ++ show m4
    g <- getStdGen 
    let m5 :: Int
        (m5, g2) = uniform g
    print $ "m5:" ++ show m5
    let m6 :: Int
        (m6, g3) = uniform g
    print $ show m6 ++ "Same as m5, as we are using the same StdGen instance"

    let m5 :: Int
        (m5, g2) = uniform g
    print m5
    let m6 :: Int
        (m6, g3) = uniform g
    print m6
    

data Direction = North | East | South | West deriving (Eq, Enum, Bounded, Show, Read)
data Turn = TNone | TLeft | TRight | TAround deriving (Eq, Enum, Bounded, Show, Read)


uniformIO :: (MonadIO m, Uniform a) => m a
uniformIO = getStdRandom uniform

uniformRangeIO :: (MonadIO m, UniformRange a) => (a, a) -> m a
uniformRangeIO r = getStdRandom $ uniformR r


instance {-# OVERLAPS #-} (Enum e) => UniformRange e where
    uniformRM (lo, hi) rng = do
        toEnum <$> uniformRM (fromEnum lo :: Int, fromEnum hi) rng


instance {-# OVERLAPS #-} (Enum e, Bounded e) => Uniform e where
    uniformM rng = uniformRM (minBound, maxBound) rng
