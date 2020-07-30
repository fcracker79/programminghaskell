module Main where
import Chapter1
import Chapter2
main :: IO ()
main = do {
  putStr "Quick sort: "
  ; print $ qsort [5, 4, 3, 2, 1]
  ; putStr "Factorial(5): "
  ; print $ factorial 5
}
