{-# LANGUAGE StandaloneDeriving #-}

module HaskellInDepth.CH2.Antenna where


import Fmt (Buildable(..))
class (Eq a, Enum a, Bounded a) => CyclicEnum a where
    cpred :: a -> a
    cpred d
        | d == minBound = maxBound
        | otherwise = pred d
    csucc :: a -> a
    csucc d
        | d == maxBound = minBound
        | otherwise = succ d


data Direction = North | East | South | West deriving (Eq, Enum, Bounded, Show, Read)
data Turn = TNone | TLeft | TRight | TAround deriving (Eq, Enum, Bounded, Show, Read)

instance CyclicEnum Direction
instance CyclicEnum Turn


rotate :: Turn -> Direction -> Direction
rotate TNone = id
rotate TLeft = cpred
rotate TRight = csucc
rotate TAround = cpred . cpred


every :: (Enum a, Bounded a) => [a]
every = enumFrom minBound

orient :: Direction -> Direction -> Turn
orient d1 d2 = head $ filter (\t -> rotate t d1 == d2) every

rotateMany :: Direction -> [Turn] -> Direction
rotateMany = foldl (flip rotate)

rotateMany' :: Direction -> [Turn] -> Direction
rotateMany' dir ts = rotate (mconcat ts) dir

rotateManySteps :: Direction -> [Turn] -> [Direction]
rotateManySteps = scanl (flip rotate)

orientMany :: [Direction] -> [Turn]
orientMany ds@(_:_:_) = zipWith orient ds (tail ds)
orientMany _ = []


instance Semigroup Turn where
    TNone <> t = t
    TLeft <> TLeft = TAround
    TLeft <> TRight = TNone
    TLeft <> TAround = TRight
    TRight <> TRight = TAround
    TRight <> TAround = TLeft
    TAround <> TAround = TNone
    t1 <> t2 = t2 <> t1


instance Monoid Turn where
    mempty = TNone


instance Buildable Direction where
    build North = "N"
    build East = "E"
    build South = "S"
    build West = "W"

instance Buildable Turn where
    build TNone = "--"
    build TLeft = "<-"
    build TRight = "->"
    build TAround = "||"
