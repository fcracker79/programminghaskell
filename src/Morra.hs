module Morra(playWithComputer, MorraState, initialState) where


import Data.Map(Map)
import Control.Monad.Trans.State.Lazy(StateT, get, put)
import System.Random ( newStdGen, Random(randomR), StdGen )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Data.Maybe ( fromMaybe )
import Text.Read(readMaybe)
import qualified Data.Map as Map
import Control.Monad (guard)

data MorraState = MorraState { computerIsEven :: Bool, playerScore :: Int, computerScore :: Int, latestPlayerChoices :: [Int], recordedPlayerChoices :: Map (Int, Int) Int }

initialState :: IO MorraState
initialState = do
    g <- newStdGen
    let (computerChoice, _) = randomR (1, 10) g :: (Int, StdGen)
    return $ MorraState {
        computerIsEven = even computerChoice,
        playerScore = 0,
        computerScore = 0,
        latestPlayerChoices = [],
        recordedPlayerChoices = Map.empty 
}

msg :: Show a => a -> StateT MorraState IO ()
msg = liftIO . print


getPlayerChoice :: StateT MorraState IO Int
getPlayerChoice = do
    msg "Take a choice"
    playerChoiceString <- liftIO getLine
    let playerChoice = readMaybe playerChoiceString :: (Maybe Int)
    fromMaybe <$> getPlayerChoice <*> pure playerChoice 


guessComputerChoice :: StateT MorraState IO (Maybe Int)
guessComputerChoice = do
    state <- get 
    let latestChoices = latestPlayerChoices state
    guard $ length (latestPlayerChoices state) >= 2
    let key = (latestChoices !! (length latestChoices - 2), latestChoices !! (length latestChoices - 1)) 
    return $ Map.lookup key (recordedPlayerChoices state)


getRandomComputerChoice :: StateT MorraState IO Int
getRandomComputerChoice = do
    g <- liftIO newStdGen
    let (computerChoice, _) = randomR (1, 10) g :: (Int, StdGen)
    return computerChoice

getComputerChoice :: StateT MorraState IO Int
getComputerChoice = fromMaybe <$> getRandomComputerChoice <*> guessComputerChoice


nextPlayerChoices :: Int -> [Int] -> [Int]
nextPlayerChoices d [a, b, c] = [b, c, d]
nextPlayerChoices a v = a:v


recordNewPlayerChoices :: [Int] -> Map (Int, Int) Int -> Map (Int, Int) Int
recordNewPlayerChoices [a, b, c] m = Map.insert (a, b) c m
recordNewPlayerChoices _ m = m


computerWins :: Int -> StateT MorraState IO ()
computerWins i = increaseWinner i 0 1

playerWins :: Int -> StateT MorraState IO ()
playerWins i = increaseWinner i 1 0


increaseWinner :: Int -> Int -> Int -> StateT MorraState IO ()
increaseWinner i playerDelta computerDelta = do
    state <- get
    let newLatestPlayerChoices = nextPlayerChoices i $ latestPlayerChoices state
    put MorraState { 
        computerIsEven = computerIsEven state,
        playerScore = playerScore state + playerDelta,
        computerScore = computerScore state + computerDelta,
        latestPlayerChoices = newLatestPlayerChoices,
        recordedPlayerChoices = recordNewPlayerChoices newLatestPlayerChoices $ recordedPlayerChoices state 
    }

findWinner :: StateT MorraState IO (Maybe String)
findWinner = do
    state <- get
    return $ if playerScore state >= 10 then Just "Congratulations, player!" else if computerScore state >= 10 then Just "Sorry, computer wins" else Nothing

playWithComputer :: StateT MorraState IO String
playWithComputer = do
    state <- get
    computerChoice <- getComputerChoice
    playerChoice <- getPlayerChoice
    let isEven = even $ playerChoice + computerChoice
    if isEven == computerIsEven state then computerWins playerChoice else playerWins playerChoice
    state <- get
    fromMaybe <$> playWithComputer <*> findWinner
