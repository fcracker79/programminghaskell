{-# LANGUAGE ScopedTypeVariables #-}
module HaskellInDepth.CH4.ExerciseComonadFoldPaolino where

import qualified Control.Foldl as L
import qualified Control.Comonad as C
import Control.Comonad.Traced
import Control.Monad


-- blend xs ys zs =  (somma (xs <> zs), media (ys <> zs)) consumando zs una sola volta
blendWithoutComonadIamSoSorry :: [Double]-> [Double] -> [Double] -> (Double, Double)
blendWithoutComonadIamSoSorry xs ys zs = (sumzs + L.fold L.sum xs, (sumys + sumzs) / fromIntegral (lengthys + lengthzs))
    where (sumzs, lengthzs) = L.fold sumAndLength zs
          (sumys, lengthys) = L.fold sumAndLength ys
          sumAndLength :: L.Fold Double (Double, Int)
          sumAndLength = (,) <$> L.sum <*> L.length

blend :: [Double]-> [Double] -> [Double] -> (Double, Double)
blend xs ys zs = (L.fold s zs, L.fold m zs)
    where s = C.extend (\w -> L.fold w xs) L.sum
          m  = C.extend (\w -> L.fold w ys) L.mean


sampleMirkoWithReset :: forall a b. Int -> L.Fold a b -> [a] -> [b]
sampleMirkoWithReset _ f [] = []
sampleMirkoWithReset i f xs = let (x0,x1) = splitAt i xs 
                                  newf = C.extend (\w -> L.fold w x0) f
                                  in extract newf : sampleMirkoWithReset i f x1

sampleMirko :: forall a b. Int -> L.Fold a b -> [a] -> [b]
sampleMirko _ f [] = []
sampleMirko i f xs = let (x0,x1) = splitAt i xs 
                         newf = C.extend (\w -> L.fold w x0) f
                         in extract newf : sampleMirko i newf x1

sampleT :: forall a b. Int -> L.Fold a b -> L.Fold a [b]
sampleT n (L.Fold step begin end) = L.Fold newstep (0, [begin]) newend
  where newstep (count, x:xs) a
          | count == n - 1 = (0, begin : step x a : xs)
          | otherwise = (count + 1, step x a : xs)
        newstep (_, []) _ = (0, [])
        newend (0, x:xs) = reverse $ fmap end xs
        newend (_, statuses) = reverse $ fmap end statuses

samplePaolino :: Int -> L.Fold a b -> [a] -> [b]
samplePaolino n = go
  where
    go _ [] = []
    go x ys =
      let (zs, ys') = splitAt n ys
          x' = x =>> flip L.fold zs
       in extract x' : go x' ys'

main :: IO ()
main = do
    let arr = [1,2,3,4,5,6]
    putStrLn $ "Array: " ++ show arr

    let result = sampleMirko 2 L.sum arr
    putStrLn $ "Using sampleMirko: " ++ show result

    let result = sampleMirkoWithReset 2 L.sum arr
    putStrLn $ "Using sampleMirkoWithReset: " ++ show result

    let result = samplePaolino 2 L.sum arr
    putStrLn $ "Using samplePaolino: " ++ show result

    let sample = sampleT 3 L.sum
    let sampled = L.fold sample arr
    putStrLn $ "Trying sampleT: " ++ show sampled
    return ()


data Answer = Low | High | Flag Int 

-- how to box the challange
exact :: Int -> Traced (Sum Int) Answer 
exact n = traced $ \(Sum m) -> case compare n m of 
  EQ -> Flag n
  GT -> Low 
  LT -> High 

-- write a guesser so that guess . exact == identity
guess :: Traced (Sum Int) Answer -> Int 
guess f = undefined 