module Main where
import Chapter1
import Chapter2
import Tree
main :: IO ()
main = do {
  putStr "Quick sort: "
  ; print $ qsort [5, 4, 3, 2, 1]
  ; putStr "Factorial(5): "
  ; print $ factorial 5
  ; putStrLn "Tree of height 0"
  ; printTree 0
  ; putStrLn "Tree of height 1"
  ; printTree 1
  ; putStrLn "Tree of height 2"
  ; printTree 2
  ; putStrLn "Tree of height 3"
  ; printTree 3
  ; putStrLn "Tree of height 4"
  ; printTree 4
  ; putStrLn "Tree of height 5"
  ; printTree 5
}
