module HaskellProgrammingFromFirstPrinciples.PlayWithMaps where

import Data.Map ( fromList, Map )
data Dino = Dino {dino :: Int, gino :: Int} deriving (Eq, Ord)


l :: [Int] -> [(Dino, Int)]
l = fmap (\x -> (Dino x ((x * x)::Int), x))

m :: Map Dino Int
m = fromList $ l [0..1000]