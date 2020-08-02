module Main where
import Chapter1
import Chapter2
import Chapter4
import ChristmasTree
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
  ; putStrLn "Even and odd for [1,2,3,4,5]"
  ; print $ evenOdd [1,2,3,4,5]
  ; putStrLn "Halve [1,2,3,4,5]"
  ; print $ halve [1,2,3,4,5]
  ; print $ third1 [1,2,3,4,5]
  ; print $ third2 [1,2,3,4,5]
  ; print $ third3 [1,2,3,4,5]
  ; print $ safetailConditionalExpression [1,2,3,4,5]
  ; print $ safetailGuardedEquation [1,2,3,4,5]
  ; print $ safetailPatternMatching [1,2,3,4,5]
  ; print $ safetailConditionalExpression ([] :: [Int])
  ; print $ safetailGuardedEquation ([] :: [Int])
  ; print $ safetailPatternMatching ([] :: [Int])
  ; print $ luhnDouble 3
  ; print $ luhnDouble 4
  ; print $ luhnDouble 6
  ; print $ luhn ([1, 7, 8, 4])
  ; print $ luhn ([4, 7, 8, 3])
}
