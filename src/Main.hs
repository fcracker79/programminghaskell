module Main where
import Chapter1
import Chapter2
import Chapter4
import Chapter5
import Chapter6
import ChristmasTree
main :: IO ()
main = do {
    print $ firstAndLast [1..5]
    ; print $ position [1, 2, 3, 3, 4, 3, 4, 5] 3
}
