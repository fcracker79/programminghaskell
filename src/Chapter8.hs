module Chapter8 where

type Position = (Double, Double)
distance :: Position -> Double
distance (x,y) = sqrt(x * x + y * y)

data Movement = North | South | East | West
onemove :: Movement -> Position -> Position
onemove North (x,y) = (x, y - 1)
onemove South (x,y) = (x, y + 1)
onemove East (x,y) = (x + 1, y)
onemove West (x,y) = (x - 1, y)

move :: [Movement] -> Position -> Position
move [] p = p
move (x:xs) p = move xs (onemove x p)
