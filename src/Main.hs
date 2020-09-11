module Main where
import Chapter1
import Chapter2
import Chapter4
import Chapter5
import Chapter7
import Chapter8
import Chapter9
import Chapter10
import Chapter12
import Chapter13
import ChristmasTree

import Data.Char
main :: IO ()
main = do print $ mySum [1,2,3, 4]
          print $ myProd [1, 2, 3, 4]
          print $ myLength [11,22,33,44,55,66]
          print $ myAll (\x -> x > 5) [6,7,8,9]
          print $ myAll (\x -> x > 5) [6,7,8,2]
          print $ myTakeWhile (\x -> x > 5) [6,7,8,9, 1, 7, 9]
          print $ myDropWhile (\x -> x > 5) [6,7,8,9, 1, 7, 9]
          print $ myMap (+ 1) [1,2,3]
          print $ myFilter (> 5) [6,7,8,9, 1, 7, 9]
          print $ dec2int [1,2,3]
          print $ myUncurry (+) $ (1, 2)
          print $ (myCurry (myUncurry (+))) 1 2
          print $ int2bin 126
          print $ chop8 [1,2,3,4,5,6,7,8,9,0]
          print $ chop8unfold [1,2,3,4,5,6,7,8,9,0]
          print $ mapunfold (+ 1) [1,2,3,4]
          print $ take 10 (iterateunfold (* 2) 1)
          print $ altmap (+10) (+100) [0,1,2,3,4]
          print $ luhnaltmap [1,7,8,4]
          print $ luhnaltmap [4,7,8,3]
          print $ distance (3,4)
          print $ move [North, East, East] (3,4)
          print $ Succ (Succ ( Succ ( Succ (Zero))))
          print $ Succ (Succ ( Succ ( Succ ( Succ ( Succ(Zero))))))
          print (addnat (Succ Zero) (Succ Zero))
          print $ Node (Node (Leaf 3) 1 (Leaf 4)) 0 (Node (Leaf 5) 2 (Leaf 6))
          print $ leaves (Node (Node (Leaf 3) 1 (Leaf 4)) 0 (Node (Leaf 5) 2 (Leaf 6)))
          print $ hello "This is a normal string"
          print $ hello (12345::Int)
          print $ hello True
          print $ nat2int (mulnat (int2nat 3) (int2nat 4))
          print $ balanced (Node2 (Node2 (Leaf2 3) (Leaf2 4)) (Node2 (Leaf2 5) (Leaf2 6)))
          print $ balanced (Node2 (Node2 (Leaf2 3) (Leaf2 4)) (Node2 (Leaf2 5) (Node2 (Leaf2 6) (Leaf2 7))))
          print $ balanced (Node2 (Node2 (Leaf2 3) (Leaf2 4)) (Node2 (Leaf2 5) (Node2 (Leaf2 6) (Node2 (Leaf2 7) (Leaf2 8)))))
          print $ balanced (balanced_tree [1,2,3,4,5,6,7,8])
          print $ size (Add (Val 1) (Add (Val 2) (Val 3)))
          print $ Appo Addo (Valo 1) (Appo Mulo (Valo 2) (Valo 3))
          print $ evalo (Appo Addo (Valo 1) (Appo Mulo (Valo 2) (Valo 3)))
          print $ evalo (Appo Divo (Valo 1) (Valo 0))
          print $ solution (Appo Mulo (Appo Addo (Valo 1) (Valo 50)) (Appo Subo (Valo 25) (Valo 10))) [1,3,7,10,25,50] 765
          -- IDE buffers input and it starts working oddly.
          -- chars <- getTwoOfThreeChars
          -- print $ chars
          -- line <- getLine2
          -- print $ line
          -- playWord "foo"
          print $ gameOfLife $ Board
            [DeadCell, LivingCell, DeadCell,
             LivingCell, DeadCell, LivingCell,
             LivingCell, LivingCell, DeadCell
            ]
            (3, 3)
          -- print $ neighbours (Board
          --                                        [DeadCell, LivingCell, DeadCell,
          --                                         LivingCell, DeadCell, LivingCell,
          --                                         LivingCell, LivingCell, DeadCell
          --                                        ]
          --                                        (3, 3)) (0, 1)
          -- dino <- myGetChars 10
          -- print $ dino
          -- myx <- myGetInt 4
          -- print $ "The number is " ++ show myx

          print $ parse item "abc"
          print $ parse item ""
          print $ parse (fmap toUpper item) "abc"
          print $ parse (fmap toUpper item) ""