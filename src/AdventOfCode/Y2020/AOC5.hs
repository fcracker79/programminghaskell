module AdventOfCode.Y2020.AOC5 where

import Control.Monad.Trans.State.Lazy(State, execState)
import Control.Monad.State.Class(get, put)
import Control.Applicative(empty, many)
import Control.Monad(guard)
import qualified Control.Monad.Trans.Maybe as M
import qualified Misc.MyParser as P
import Debug.Trace(trace)

data FrontOrBack = F | B deriving(Show, Eq)
data LeftOrRight = L | R deriving(Show, Eq)

data BoardingPassState = BoardingPassState { 
    startRow :: Int, endRow :: Int, 
    startCol :: Int, endCol :: Int,
    rows :: [FrontOrBack],
    columns :: [LeftOrRight]
    } deriving(Show)


processBoardingPassState :: M.MaybeT (State BoardingPassState) BoardingPassState
processBoardingPassState = do
    currentState <- get
    let curRows = rows currentState
    let curColumns = columns currentState
    case curRows of
        (x:xs) -> updateRows x
        [] -> case curColumns of
              (y:ys) -> updateColumns y
              [] -> empty


updateColumns :: LeftOrRight -> M.MaybeT (State BoardingPassState) BoardingPassState
updateColumns fb = do
    currentState <- get
    let currentEndCol = endCol currentState
    let currentStartCol = startCol currentState
    let newValueUpper = (currentEndCol + currentStartCol + 1) `quot` 2 
    let newValueLower = (currentEndCol + currentStartCol) `quot` 2 
    let newState = BoardingPassState {
        startRow = startRow currentState,
        endRow = endRow currentState, 
        startCol = if fb == R then newValueUpper else currentStartCol,
        endCol = if fb == L then newValueLower else currentEndCol,
        columns = tail $ columns currentState,
        rows = rows currentState
    }
    put $ trace ("updateColumns " ++ show newState) newState
    return newState 


updateRows :: FrontOrBack -> M.MaybeT (State BoardingPassState) BoardingPassState
updateRows fb = do
    currentState <- get
    let currentEndSeat = endRow currentState
    let currentStartSeat = startRow currentState
    let newValueUpper = (currentEndSeat + currentStartSeat + 1) `quot` 2 
    let newValueLower = (currentEndSeat + currentStartSeat) `quot` 2 
    let newState = BoardingPassState { 
        startRow = if fb == B then newValueUpper else currentStartSeat,
        endRow = if fb == F then newValueLower else currentEndSeat,
        rows = tail $ rows currentState,
        columns = columns currentState,
        startCol = startCol currentState,
        endCol = endCol currentState
    }
    put $ trace ("updateRows " ++ show newState) newState
    return newState 

parseFrontOrBack :: P.MyParser [FrontOrBack]
parseFrontOrBack = many $ do
                            c <- P.item
                            case c of
                                'F' -> return F
                                'B' -> return B
                                _ -> empty

parseLeftOrRight :: P.MyParser [LeftOrRight]
parseLeftOrRight = many $ do
                            c <- P.item
                            case c of
                                'L' -> return L
                                'R' -> return R
                                _ -> empty


data Seat = Seat { row :: Int, column :: Int, seatId :: Int} deriving(Show, Eq)


findBoardingPlace :: String -> Maybe Seat
findBoardingPlace sPosition = do
    guard (length sPosition == 10)
    (curRows, _) <- P.parse parseFrontOrBack $ take 7 sPosition
    (curColumns, _) <- P.parse parseLeftOrRight $ drop 7 sPosition
    let initialState = BoardingPassState { 
            startRow = 0,
            endRow = 127,
            rows = curRows,
            columns = curColumns,
            startCol = 0,
            endCol = 7
        }

    let processResult = execState (M.runMaybeT (many processBoardingPassState)) initialState
    return $ Seat {
            row = startRow processResult, 
            column = startCol processResult,
            seatId = startRow processResult * 8 + startCol processResult
        }
