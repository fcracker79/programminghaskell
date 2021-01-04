module AdventOfCode.Y2020.AOC3Alternative(
    initialState,
    parseArea,
    moveUntilEnd
) where


import qualified Control.Monad.Trans.State.Lazy as ST
import qualified Control.Monad.Trans.Maybe as M
import Control.Monad.State.Lazy (get, put, state)
import Control.Applicative (many, empty)
import Control.Monad(guard)
import Data.Maybe (isJust)

data Square = Tree | OpenSquare deriving(Eq)
instance Show Square where
    show Tree = "#"
    show OpenSquare = "."

type Pattern = [Square]
type Area = [Pattern]
type Position = (Int, Int)
type Movement = (Int, Int)
data TobogganState = TobogganState { area :: Area, trees :: Int, position :: Position, step :: Movement }


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

move :: M.MaybeT (ST.State TobogganState) TobogganState
move = do
    currentState <- get
    let (mx, my) = step currentState
    let _area = area currentState
    let (currentX, currentY) = position currentState
    let newTrees = trees currentState + (if getSquare (position currentState) _area == Tree then 1 else 0)
    let newPosition = (currentX + mx, currentY + my)
    if my + currentY < length (area currentState)
        then do
            let newState = TobogganState { 
                area = _area, 
                trees = newTrees, 
                position = newPosition,
                step = step currentState 
            }
            put newState
            return newState
        else do
            put TobogganState { 
                area = _area, 
                trees = newTrees, 
                position = position currentState,
                step = step currentState 
            }
            empty


moveUntilEnd :: M.MaybeT (ST.State TobogganState) Int
moveUntilEnd = do
    many move
    trees <$> get


initialState :: Movement -> Area -> TobogganState
initialState m a = TobogganState {area = a, trees = 0, position = (0, 0), step = m}
