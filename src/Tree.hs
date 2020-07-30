module Tree where

treeLayer :: (Eq a, Num a) => a -> a -> [a]

treeLayer 0 layer = [0, 0]
treeLayer height layer = [spaces, stars]
                         where
                             spaces = height - 2 - layer
                             stars = layer * 2 + 1

tree 0 = []
tree 1 = []
tree height = [(treeLayer height layer) | layer <- [0..height - 2]]

printLayer layer = do {
    ; putStr $ replicate (layer !! 0) ' '
    ; putStr $ replicate (layer !! 1) '*'
    ; putStrLn ""
}

printRoot 0 = putStrLn ""
printRoot 1 = putStrLn "|"
printRoot 2 = putStrLn "|"
printRoot height = do {
   ; putStr $ replicate (height - 2) ' '
   ; putStrLn "|"
}

printTree 0 = putStrLn ""
printTree 1 = printRoot 1
printTree height = mconcat ([printLayer layer | layer <- tree height] ++ [ printRoot height ])