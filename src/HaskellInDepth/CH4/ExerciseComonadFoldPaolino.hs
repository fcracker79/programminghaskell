{-# LANGUAGE ScopedTypeVariables #-}
module HaskellInDepth.CH4.ExerciseComonadFoldPaolino where

import qualified Control.Foldl as L
import qualified Control.Comonad as C
import Control.Comonad.Traced


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


sample :: forall a b. Int -> L.Fold a b -> [a] -> [b]
sample i f xs = L.fold newf xs 
    where 
          newf :: L.Fold a [b]
          newf = C.extend (f2 xs) f
          f2 :: [a] -> L.Fold a b -> [b]
          f2 arr _f | length arr <= i = [L.fold _f arr]
          f2 arr _f = L.fold f (take i arr): f2 (drop i arr) _f

sample2 :: forall a b. Int -> L.Fold a b -> [a] -> [b]
sample2 i f xs = L.fold newf xs 
    where 
          newf :: L.Fold a [b]
          newf = C.extend (f2 xs) f
          f2 :: [a] -> L.Fold a b -> [b]
          f2 [] _ = []
          f2 arr _f = L.fold f (take i arr): f2 (drop i arr) _f

sample3 :: forall a b. Int -> L.Fold a b -> [a] -> [b]
sample3 i f xs = L.fold newf xs 
    where 
          newf :: L.Fold a [b]
          newf = C.extend (f2 xs) f
          f2 :: [a] -> L.Fold a b -> [b]
          f2 [] _f = [L.fold _f []]
          f2 arr _f = L.fold _f (take i arr): f2 (drop i arr) _f

sample4 :: forall a b. Int -> L.Fold a b -> [a] -> [b]
sample4 i f xs = L.fold newf xs 
    where 
          newf :: L.Fold a [b]
          newf = sequenceA (f2 xs f)
          f2 [] _f = [_f]
          f2 arr _f = C.extend (\__f -> L.fold __f (take i arr)) _f: f2 (drop i arr) _f


sample5 :: forall a b. Int -> L.Fold a b -> [a] -> [b]
sample5 _ f [] = []
sample5 i f xs = let (x0,x1) = splitAt i xs 
                     newf = C.extend (\w -> L.fold w x0) f
                     in extract newf : sample5 i newf x1

-- sampleT :: Int -> L.Fold a b -> L.Fold a [b]
-- sampleT i f = newf 
--     where 
--           newf :: L.Fold a [b]
--           newf = C.extend (f2 xs) f
--           f2 :: [a] -> L.Fold a b -> [b]
--           f2 arr _f | length arr < i = [L.fold _f arr]
--           f2 arr _f = L.fold f (take i arr): f2 (drop i arr) _f


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

    let result1 = sample 2 L.sum arr
    putStrLn $ "Using sample: " ++ show result1

    let result2 = sample2 2 L.sum arr
    putStrLn $ "Using sample2: " ++ show result2

    let result3 = sample3 2 L.sum arr
    putStrLn $ "Using sample3: " ++ show result3

    let result2_bis = sample2 4 L.sum arr
    putStrLn $ "Using sample2 with 4: " ++ show result2_bis
    
    let result4 = sample4 2 L.sum arr
    putStrLn $ "Using sample4: " ++ show result4
    
    let result5 = sample5 2 L.sum arr
    putStrLn $ "Using sample5: " ++ show result5

    let resultPaolino = samplePaolino 2 L.sum arr
    putStrLn $ "Using samplePaolino: " ++ show resultPaolino
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