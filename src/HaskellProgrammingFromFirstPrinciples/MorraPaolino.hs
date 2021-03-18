{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellProgrammingFromFirstPrinciples.MorraPaolino where


import Control.Arrow (first, second)
import Control.Monad.Fix (fix)
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.Map as M
import System.Random
  ( StdGen
  , newStdGen
  , randomRIO
  , uniformR
  )
import Text.Printf (printf)
import Data.Maybe (fromMaybe)

-- a player hiding his strategy
data Player
  = Player
      Int -- next move
      (Int -> Player) -- next player state based on adversary move

type Role = Int -> Bool 
-- cps rolling 1 or 2
roll :: StdGen -> (StdGen -> Int -> a) -> a
roll g f = let (a, g') = uniformR (1 :: Int, 2) g in f g' a

-- a player which plays 2
-- h :: StdGen -> Int -> Player
-- h crea il nuovo player con la mappa delle ultime giocate aggiornata
-- g, g' :: StdGen
-- start :: Int
-- start viene passato da roll
-- m :: Map (Int, Int) Int
-- x, y, z :: Int
-- f :: StdGen -> Map (Int, Int) Int -> Int -> Int -> Int -> Player
threeGramsPlayer :: StdGen -> Role -> Player
threeGramsPlayer g role = roll g $ \g' start -> Player start $ f g' mempty 1 1
  where
    f g m x y z = case M.lookup (x, y) m of
      Nothing -> roll g h
      Just z' -> h g $ if role $ z' + 1 then 1 else 2
      where
        h g x = Player x $ f g (M.insert (x, y) z m) y z

{-
game :: Role -> Player -> IO ()
game winning player =
  (player , 0, 0, 0) & fix
    do
      \go (player, iWon :: Int, youWon :: Int, games :: Int) -> do
        printf "\ntoss > "
        you <- readLn
        let Player me player' = player
            won = winning $ you + me
            [meR, youR] = if won then "! " else " !"
            (iWon', youWon') = (if won then first else second) succ (iWon, youWon)
            games' = succ games
        printf "%cMe: %d, %cYou: %d, I won: %d/%d, You won %d/%d" meR me youR you iWon' games' youWon' games'
        go (player' you, iWon', youWon', games')
-}

playMostFrequent :: StdGen -> Role -> Player
playMostFrequent g role = roll g $ \g' start -> Player start $ f g' mempty
  where f g m v = Player nextMove $ f g nextState
                  where 
                        nextState = M.insert v (lastNumMoves + 1) m
                        prev1 = fromMaybe 0 (M.lookup 1 m)
                        prev2 = fromMaybe 0 (M.lookup 2 m)
                        mostFrequentMove = if prev1 > prev2 then 1 else 2
                        lastNumMoves = fromMaybe 0 (M.lookup v m)
                        nextMove = if role (mostFrequentMove + 1) then 1 else 2

game2 :: Role -> Player -> IO ()
game2 winning player = go (player , 0, 0, 0)
    where go (player, iWon :: Int, youWon :: Int, games :: Int) = do
          printf "\ntoss > "
          you <- readLn
          let Player me player' = player
              won = winning $ you + me
              [meR, youR] = if won then "! " else " !"
              (iWon', youWon') = (if won then first else second) succ (iWon, youWon)
              games' = succ games
          printf "%cMe: %d, %cYou: %d, I won: %d/%d, You won %d/%d" meR me youR you iWon' games' youWon' games'
          go (player' you, iWon', youWon', games')

main :: IO ()
main = do
  printf "want [E]ven or [O]dd? > "
  eo <- getLine
  g <- newStdGen 
  let role = case eo of
        "E" -> even
        "O" -> odd
  -- let computerStrategy = threeGramsPlayer g
  let computerStrategy = playMostFrequent g
  game2 (not . role) $ computerStrategy (not . role)
