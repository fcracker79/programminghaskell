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

move :: Movement -> ST.State TobogganState ()
move (mx, my) = do
    currentState <- ST.get
    let _area = area currentState
    let (currentX, currentY) = position currentState
    let newPosition = (currentX + mx, currentY + my)
    ST.put TobogganState { area = _area, trees = trees currentState, position = newPosition }
    return ()


endOfArea :: Movement -> ST.State TobogganState Bool
endOfArea m = do
    currentState <- ST.get
    let (_, my) = m
    let (_, y) = position currentState
    return (my + y >= length (area currentState))


moveUntilEnd :: Movement -> ST.State TobogganState Int
moveUntilEnd m = do
    currentState <- ST.get
    let _area = area currentState
    let _position = position currentState
    let newTrees = trees currentState + (if getSquare (position currentState) _area == Tree then 1 else 0)
    ST.put TobogganState { area = _area, trees = newTrees, position = _position }
    _endOfArea <- endOfArea m
    if trace ("end of area?" ++ show _endOfArea)_endOfArea then return (trees currentState) 
    else do
         move $ (trace ("moving again, current position" ++ show (position currentState))) m 
         moveUntilEnd m


initialState :: Area -> TobogganState
initialState a = TobogganState {area = a, trees = 0, position = (0, 0)}
