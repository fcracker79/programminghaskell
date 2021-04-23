module HaskellInDepth.CH2.Relevants where


import Fmt
import System.Random
data Dino = Dino1 | Dino2 deriving(Show)


instance Buildable Dino where
    build Dino1 = "D1"
    build Dino2 = "D2"


-- This updates the random generator automatically
f :: IO Int
f = getStdRandom uniform

-- This updates the random generator automatically
g :: IO Int
g = getStdRandom $ uniformR (10, 20)


-- Generates independent random generators
splitGenerators :: IO (StdGen, StdGen)
splitGenerators = split <$> getStdGen


main :: IO()
main = do
    fmt $ "This will be 'Dino1': " +|| Dino1 ||+ ", as from Show\n"
    fmt $ "This will be 'D1': "+| Dino1 |+ ", as from Buildable\n"


