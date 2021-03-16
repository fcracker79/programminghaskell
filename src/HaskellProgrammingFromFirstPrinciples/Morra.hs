module HaskellProgrammingFromFirstPrinciples.Morra(playWithComputer, playWithFriend, MorraState, initialState) where


import Data.Map(Map)
import Control.Monad.Trans.State.Lazy(StateT, get, put, evalStateT)
import System.Random ( newStdGen, Random(randomR), StdGen )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Data.Maybe ( fromMaybe )
import Text.Read(readMaybe)
import qualified Data.Map as Map
import Control.Monad (guard)
import Control.Applicative((<|>))
import Text.Printf ( printf )

data MorraState = MorraState { player1IsEven :: Bool, player1Score :: Int, player2Score :: Int, latestPlayer2Choices :: [Int], recordedPlayer2Choices :: Map (Int, Int) Int }
data Player = Player1 | Player2 deriving(Eq)
type PlayersNames = (String, String)
type PlayerAction = StateT MorraState IO Int

rounds :: Int
rounds = 10


initialState :: IO MorraState
initialState = do
    player1Choice <- fmap fst $ randomR (1 :: Int, 2) <$> newStdGen
    return $ MorraState {
        player1IsEven = even player1Choice,
        player1Score = 0,
        player2Score = 0,
        latestPlayer2Choices = [],
        recordedPlayer2Choices = Map.empty 
}

msg :: Show a => a -> StateT MorraState IO ()
msg = liftIO . print

getPlayerChoice :: String -> PlayerAction
getPlayerChoice playerName = do
    msg $ "Take a choice, " ++ playerName
    playerChoiceString <- liftIO getLine
    maybe (getPlayerChoice playerName) pure $ playerChoice playerChoiceString 
    where playerChoice s = (readMaybe s :: (Maybe Int)) >>= \x -> if x >=1 && x <= 2 then Just x else Nothing


getComputerChoice :: PlayerAction
getComputerChoice = do
    state <- get
    liftIO $ fromMaybe getRandomComputerChoice $ guessComputerChoice state
    where guessComputerChoice state = do
              let latestChoices = latestPlayer2Choices state
              guard $ length latestChoices >= 2
              let key = (latestChoices !! (length latestChoices - 2), latestChoices !! (length latestChoices - 1)) 
              let maybeWinningValue = (\i -> if player1IsEven state then i else (i + 1) `mod` 2) <$> Map.lookup key (recordedPlayer2Choices state)
              pure <$> maybeWinningValue
          getRandomComputerChoice = do fmap fst $ randomR (1 :: Int, 2) <$> newStdGen

increaseWinner :: Player -> Int -> String -> StateT MorraState IO ()
increaseWinner winner lastChoicePlayer2 playerName = do
    msg $ "Player " ++ playerName ++ " wins"
    state <- get
    let newLatestPlayerChoices = nextPlayerChoices lastChoicePlayer2 $ latestPlayer2Choices state
    put MorraState { 
        player1IsEven = player1IsEven state,
        player1Score = player1Score state + if winner == Player1 then 1 else 0,
        player2Score = player2Score state + if winner == Player2 then 0 else 1,
        latestPlayer2Choices = newLatestPlayerChoices,
        recordedPlayer2Choices = recordNewPlayerChoices newLatestPlayerChoices $ recordedPlayer2Choices state 
    }
    where nextPlayerChoices d [a, b, c] = [b, c, d]
          nextPlayerChoices a v = a:v
          recordNewPlayerChoices [a, b, c] m = Map.insert (a, b) c m
          recordNewPlayerChoices _ m = m

findWinner :: StateT MorraState IO (Maybe Player)
findWinner = do
    state <- get
    (guard (player1Score state >= rounds) >> return (Just Player1)) <|>
        (guard (player2Score state >= rounds) >> return (Just Player2)) <|>
        return Nothing

playMorraM :: PlayersNames -> PlayerAction -> PlayerAction -> StateT MorraState IO Player
playMorraM playersNames player1Choice player2Choice = do
    let (p1Name, p2Name) = playersNames
    state <- get
    p1Choice <- player1Choice
    p2Choice <- player2Choice
    liftIO $ printf "%s chooses %d\n%s chooses %d\n" p1Name p1Choice p2Name p2Choice
    let isEven = even $ p1Choice + p2Choice
    if isEven == player1IsEven state then player1Wins p2Choice p1Name else player2Wins p2Choice p2Name
    maybeWinner <- findWinner
    case maybeWinner of
        Nothing -> playMorraM playersNames player1Choice player2Choice
        Just Player1 -> do
            msg $ fst playersNames ++ " is the final winner!"
            return Player2
        Just Player2 -> do
            msg $ snd playersNames ++ " is the final winner!"
            return Player2
    where player1Wins = increaseWinner Player1
          player2Wins = increaseWinner Player2

playMorra :: PlayersNames -> PlayerAction -> PlayerAction -> IO Player
playMorra playersNames p1Choice p2Choice = do
    is <- initialState
    let p1Even = player1IsEven is
    let p1Role = if p1Even then "even" else "odd" :: String
    let p2Role = if p1Even then "odd" else "even" :: String
    
    printf "%s plays %s\n%s plays %s\n" (fst playersNames) p1Role (snd playersNames) p2Role
    evalStateT (playMorraM playersNames p1Choice p2Choice) is

playWithComputer :: IO Player
playWithComputer = playMorra ("Computer", "Player 1") getComputerChoice $ getPlayerChoice "Player 1" 

playWithFriend :: IO Player
playWithFriend = playMorra ("Player 1", "Player 2") (getPlayerChoice "Player 1") (getPlayerChoice "Player 2")
