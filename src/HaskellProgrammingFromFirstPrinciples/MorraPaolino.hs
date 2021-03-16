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
three_grams_player :: Role -> IO Player
three_grams_player role = do
  g <- newStdGen
  pure $ roll g $ \g' start -> Player start $ f g' mempty 1 1
  where
    f g m x y z = case M.lookup (x, y) m of
      Nothing -> roll g h
      Just z' -> h g $ if role $ z' + 1 then 1 else 2
      where
        h g x = Player x $ f g (M.insert (x, y) z m) y z

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

main :: IO ()
main = do
  printf "want [E]ven or [O]dd? > "
  eo <- getLine
  let role = case eo of
        "E" -> even
        "O" -> odd
  three_grams_player (not . role) >>=  game (not . role)
