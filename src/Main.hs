module Main where
import Chapter1
import Chapter2
import Chapter4
import Chapter5
import Chapter7
import Chapter8
import ChristmasTree
main :: IO ()
main = do {
    print $ mySum [1,2,3, 4]
    ;print $ myProd [1, 2, 3, 4]
    ;print $ myLength [11,22,33,44,55,66]
    ; print $ myAll (\x -> x > 5) [6,7,8,9]
    ; print $ myAll (\x -> x > 5) [6,7,8,2]
    ; print $ myTakeWhile (\x -> x > 5) [6,7,8,9, 1, 7, 9]
    ; print $ myDropWhile (\x -> x > 5) [6,7,8,9, 1, 7, 9]
    ; print $ myMap (+ 1) [1,2,3]
    ; print $ myFilter (> 5) [6,7,8,9, 1, 7, 9]
    ; print $ dec2int [1,2,3]
    ; print $ myUncurry (+) $ (1, 2)
    ; print $ (myCurry (myUncurry (+))) 1 2
    ; print $ int2bin 126
    ; print $ chop8 [1,2,3,4,5,6,7,8,9,0]
    ; print $ chop8unfold [1,2,3,4,5,6,7,8,9,0]
    ; print $ mapunfold (+ 1) [1,2,3,4]
    ; print $ take 10 (iterateunfold (* 2) 1)
    ; print $ altmap (+10) (+100) [0,1,2,3,4]
    ; print $ luhnaltmap [1,7,8,4]
    ; print $ luhnaltmap [4,7,8,3]
    ; print $ distance (3,4)
    ; print $ move [North, East, East] (3,4)
}
