module Chapter10 where


getTwoOfThreeChars :: IO (Char, Char)
getTwoOfThreeChars = do x <- getChar
                        getChar
                        y <- getChar
                        return (x, y)


getLine2 :: IO String
getLine2 = do x <- getChar
              if x == '\n' then return []
              else do x <- getLine
                      return x


playWord :: String -> IO ()
playWord word = do nicetry <- getLine
                   if nicetry == word then putStrLn "You won!"
                   else do putStrLn "Try Again..."
                           playWord word


data Cell = DeadCell | LivingCell deriving Show
data BoardDefinition = Board [Cell] (Int, Int)

getCell (Board cells (x0, y0)) (x,y) =
    cells !! i
    where i = xn + x0 * yn
          xn = if x >= 0 then x else x0 + x
          yn = if y >= 0 then y else y0 + y

neighbours :: BoardDefinition -> (Int, Int) -> [Cell]
neighbours b (x0, y0) =
    [ getCell b (x0 - 1, y0)
    ,getCell b (x0, y0 - 1)
    ,getCell b (x0 - 1, y0 - 1)
    ,getCell b (x0 - 1, y0)
    ,getCell b (x0, y0 + 1)
    ,getCell b (x0 + 1, y0 + 1)
    ,getCell b (x0 - 1, y0 + 1)
    ,getCell b (x0 + 1, y0 - 1)
    ]


takeSlice :: [a] -> Int -> Int -> [a]
takeSlice elements from0 to0 = take (to0 - from0) (drop from0 elements)

show0 :: BoardDefinition -> Int -> String
show0 (Board [] (_,_)) _ = ""
show0 (Board _ (0,_)) _ = ""
show0 (Board _ (_,0)) _ = ""
show0 (Board cells (x,y)) 1 = concat (map show (takeSlice cells 0 x))
show0 (Board cells (x,y)) x1 =
    show0 (Board cells (x, y)) (x1 - 1) ++
    "\n" ++
    concat (map show (takeSlice cells (x * x1) (x * (x1 + 1))))

instance Show BoardDefinition where
   show (Board cells (x,y)) = show0 (Board cells (x,y)) (x - 1)
