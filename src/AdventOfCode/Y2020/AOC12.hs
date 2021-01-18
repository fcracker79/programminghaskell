module AdventOfCode.Y2020.AOC12 where

import qualified Misc.MyParser as P
import Control.Applicative(empty, many)
import Control.Monad(guard)
import Control.Monad.State
import Debug.Trace(trace)


data Direction = North | South | East | West deriving(Show, Eq)

data Action = Turn Int | Move Direction Int | MoveForward Int deriving(Show, Eq)



parseTurning :: Int -> P.MyParser Action
parseTurning sign = do
    n <- P.integer
    guard (n `mod` 90 == 0)
    return $ Turn (n `quot` 90 * sign)

parseMove :: Direction -> P.MyParser Action
parseMove d = do
    Move d <$> P.integer


parseAction :: P.MyParser Action
parseAction = do
    P.trimming
    c <- P.item
    case c of
        'R' -> parseTurning 1
        'L' -> parseTurning (-1)
        'N' -> parseMove North
        'S' -> parseMove South
        'E' -> parseMove East
        'W' -> parseMove West
        'F' -> MoveForward <$> P.nat
        _   -> empty

parseActions :: P.MyParser [Action]
parseActions = many parseAction


turn :: Direction -> Action -> Direction
turn d (Move _ _) = d
turn d (MoveForward _) = d
turn d (Turn 0) = d
turn d (Turn 1)
    | d == North = East
    | d == South = West
    | d == East = South
    | d == West = North
turn d (Turn i)
    | i < 0 || i > 4 = turn d (Turn ((i `mod` 4 + 4) `mod` 4))
    | otherwise = turn (turn d (Turn 1)) (Turn (i - 1))

move :: Direction -> (Int, Int, Int, Int) -> Action -> (Int, Int, Int, Int)
move _ p (Turn _) = p
move d p (MoveForward i) = move d p (Move d i) 
move _ (n, s, e, w) (Move d i) = case d of
    North -> (n + i, s, e, w)
    South -> (n, s + i, e, w)
    East -> (n, s, e + i, w)
    West -> (n, s, e, w + i)


manhattanDistance :: Direction -> [Action] -> (Int, Int, Int, Int) -> Int
manhattanDistance _ [] (n, s, e, w) = abs (n - s) + abs (e - w)
manhattanDistance direction (a:as) position = trace (show newDirection ++ ", " ++ show newPosition) $ manhattanDistance newDirection as newPosition
    where newDirection = turn direction a
          newPosition = move direction position a
