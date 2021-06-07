module HaskellInDepth.CH4.ExerciseComonadGuessPaolino where


import Control.Comonad.Traced ( Sum(Sum), Traced, traced, Comonad (extract, extend), (=>>), ComonadTraced (trace) )
data Answer = Low | High | Flag Int 

-- how to box the challange
exact :: Int -> Traced (Sum Int) Answer 
exact n = traced $ \(Sum m) -> case compare n m of 
  EQ -> Flag n
  GT -> Low 
  LT -> High 

-- write a guesser so that guess . exact == identity
guess :: Traced (Sum Int) Answer -> Int 
guess f = case extract f of
    Flag n -> n
    Low -> guess $ f =>> trace (Sum 1)
    High -> guess $ f =>> trace (Sum (-1))