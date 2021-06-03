module HaskellInDepth.CH4.ExerciseComonadFoldPaolino where

import qualified Control.Foldl as L
import qualified Control.Comonad as C


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


-- sample :: Int -> L.Fold a b -> [a] -> [b]
-- sample i f xs = undefined 
--     where f = C.extend f2
--           f2 w = C.extract w