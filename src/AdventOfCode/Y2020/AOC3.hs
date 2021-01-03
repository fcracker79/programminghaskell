module AdventOfCode.Y2020.AOC3 where


import qualified Control.Monad.Trans.State.Lazy as ST
import Debug.Trace(trace)

data Square = Tree | OpenSquare deriving(Eq)
instance Show Square where
    show Tree = "#"
    show OpenSquare = "."

type Pattern = [Square]
type Area = [Pattern]
type Position = (Int, Int)
type Movement = (Int, Int)
data TobogganState = TobogganState { area :: Area, trees :: Int, position :: Position }


getSquare :: Position -> Area -> Square
getSquare (x, y) a = row !! (x `mod` length row) where row = a !! y


parseSquare :: Char -> Maybe Square
parseSquare s
    | s == '#' = Just Tree
    | s == '.' = Just OpenSquare
    | otherwise = Nothing

parsePattern :: String -> Maybe Pattern
parsePattern = mapM parseSquare

parseArea :: [String] -> Maybe Area
parseArea = mapM parsePattern

move :: Movement -> ST.State TobogganState Bool
move (mx, my) = do
    currentState <- ST.get
    let _area = area currentState
    let (currentX, currentY) = position currentState
    let newPosition = (currentX + mx, currentY + my)
    if my + currentY < length (area currentState)
        then do
            ST.put TobogganState { area = _area, trees = trees currentState, position = newPosition }
            return True
        else return False


moveUntilEnd :: Movement -> ST.State TobogganState Int
moveUntilEnd m = do
    currentState <- ST.get
    let _area = area currentState
    let _position = position currentState
    let newTrees = trees currentState + (if getSquare (position currentState) _area == Tree then 1 else 0)
    let sticazzi = trace ("new trees " ++ show newTrees) ()
    ST.put TobogganState { area = _area, trees = newTrees, position = _position }
    moveSuccessful <- move m 
    if moveSuccessful then moveUntilEnd m else return newTrees


initialState :: Area -> TobogganState
initialState a = TobogganState {area = a, trees = 0, position = (0, 0)}
