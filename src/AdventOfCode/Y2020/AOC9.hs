module AdventOfCode.Y2020.AOC9 where


import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State(State, evalState)
import Control.Monad.State.Class(get, put)
import Control.Monad.Trans.Maybe(MaybeT, runMaybeT)
import Control.Monad.Trans.Class(lift)
import Data.Maybe(fromMaybe)
import Control.Monad(guard, mzero)
import Control.Applicative((<|>), empty)
import Debug.Trace(trace, traceM)

data ExecutionWindow = ExecutionWindow {
        preamble :: [Int],
        allSumsCounts :: Map.Map Int Int,
        sumsByAddendum :: Map.Map Int (Set.Set Int)
    } deriving(Show)


initialState :: ExecutionWindow
initialState = ExecutionWindow {
        preamble = [],
        allSumsCounts = Map.empty,
        sumsByAddendum = Map.empty
    }

lastElement :: Monoid k => [k] -> k
lastElement [] = mempty
lastElement m = m !! (length m - 1)

insertElementIndiscriminately :: Int -> State ExecutionWindow ExecutionWindow
insertElementIndiscriminately v = do
    _state <- get
    let newPreamble = preamble _state ++ [v]
    let curSumByAddendum = Map.fromList [(x, v + x) | x <- preamble _state]
    let newSumsByAddendum0 = Map.insert v (Set.fromList (Map.elems curSumByAddendum)) (sumsByAddendum _state)
    let newSumsByAddendum = Map.unionWith 
                            Set.union
                            newSumsByAddendum0 
                            (Map.map Set.singleton curSumByAddendum)
    let deltaNewAllSumsCounts = Map.fromList $ (, 2) <$> Map.elems curSumByAddendum
    let newAllSumsCounts = Map.unionWith
                           (+)
                           (allSumsCounts _state)
                           deltaNewAllSumsCounts
    let newState = ExecutionWindow {
        preamble = newPreamble,
        allSumsCounts=newAllSumsCounts,
        sumsByAddendum=newSumsByAddendum
    }
    put newState
    return newState


decreaseSumCount :: Map.Map Int Int -> [Int] -> Map.Map Int Int
decreaseSumCount sumCount [] = sumCount
decreaseSumCount sumCount (k:ks) = decreaseSumCount newSum ks
    where newSum = Map.alter (fmap (\x -> x - 1)) k sumCount


removeElementIndiscriminately :: State ExecutionWindow ExecutionWindow
removeElementIndiscriminately = do
    _state <- get
    let (elementToRemove:newPreamble) = preamble _state
    let curSumByAddendum = Map.findWithDefault Set.empty elementToRemove (sumsByAddendum _state)
    let newSumsByAddendum = Map.delete elementToRemove $ sumsByAddendum _state
    let newAllSumsCounts = decreaseSumCount (allSumsCounts _state) $ Set.elems curSumByAddendum
    let newState = ExecutionWindow {
        preamble = newPreamble,
        allSumsCounts=newAllSumsCounts,
        sumsByAddendum=newSumsByAddendum
    }
    put newState
    return newState


addElements :: Int -> [Int] -> MaybeT (State ExecutionWindow) Int
addElements _ [] = empty
addElements preambleLength (v:vs) = do
        _state <- get
        guard $ length (preamble _state) < preambleLength
        n <- lift $ insertElementIndiscriminately v
        traceM ("after insert(0)" ++ show n)
        addElements preambleLength vs
    <|>
    do
        _state <- get
        sumsCount <- maybe mzero return $ Map.lookup v (allSumsCounts _state)
        guard $ sumsCount > 0
        n <- lift removeElementIndiscriminately
        traceM ("after remove" ++ show n)
        n2 <- lift $ insertElementIndiscriminately v
        traceM ("after insert" ++ show n2)
        addElements preambleLength vs
    <|> return v

findFirstInvalidElement :: Int -> [Int] -> Maybe Int
findFirstInvalidElement preambleLength v = evalState (runMaybeT (addElements preambleLength v)) initialState