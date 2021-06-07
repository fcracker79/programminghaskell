module HaskellInDepth.CH5.STRefExample where


import Control.Monad.ST
import Data.STRef
import Data.Foldable
import Control.Monad

comp1 :: ST s (STRef s Int)
comp1 = newSTRef 42
comp2 :: STRef s Int -> ST s Int
comp2 ref = readSTRef ref

-- Ask paolino about skolem, with reference to runST comp1


data Dino s a = Dino s a
data Dino2 s a = Dino2 s a


runDino :: (forall s. Dino s a) -> a
runDino (Dino s a) = a

runDinoNoForall :: Dino s a -> a
runDinoNoForall (Dino s a) = a


x :: Dino s (Dino2 s a)
x = undefined 

y = runDinoNoForall x
-- y = runDino x


countZerosST :: [Int] -> Int
countZerosST xs = runST $ do
    c <- newSTRef 0
    traverse_ (\x -> when (x==0) $ inc c) xs
    readSTRef c
    where
        inc c = modifySTRef' c (+1)

sumST :: [Int] -> Int
sumST xs = runST $ do
    c <- newSTRef 0
    traverse_ (add c) xs
    readSTRef c
    where add c x = modifySTRef' c (+ x)
