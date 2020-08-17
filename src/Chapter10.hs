module Chapter10 where
import Data.List
import Debug.Trace

debug = flip trace


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


data Cell = DeadCell | LivingCell deriving (Show, Eq)
data BoardDefinition = Board [Cell] (Int, Int)

getCell (Board cells (x0, y0)) (x,y) =
    cells !! i -- `debug` (show i ++ " " ++ show x ++ " " ++ show y)
    where i = xn + x0 * yn
          xn = if x >= 0 then (x `mod` x0) else x0 + x
          yn = if y >= 0 then (y `mod` y0) else y0 + y

neighbours :: BoardDefinition -> (Int, Int) -> [Cell]
neighbours b (x0, y0) =
    [ getCell b (x0 - 1, y0)
    ,getCell b (x0, y0 - 1)
    ,getCell b (x0 - 1, y0 - 1)
    ,getCell b (x0 + 1, y0)
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
show0 (Board cells (x,y)) 0 = intercalate " " (map show (takeSlice cells 0 x))
show0 (Board cells (x,y)) x1 =
    show0 (Board cells (x, y)) (x1 - 1) ++
    "\n" ++
    intercalate " " (map show (takeSlice cells (x * x1) (x * (x1 + 1))))

instance Show BoardDefinition where
   show (Board cells (x,y)) = show0 (Board cells (x,y)) (x - 1)


type CellTransformation = BoardDefinition -> (Int, Int) -> Cell

transformBoard :: CellTransformation -> BoardDefinition -> (Int, Int) -> BoardDefinition
transformBoard f (Board cells (x,y)) (xp, yp) =
    Board [
        if xp + yp * x == xc + yc * x
        then (f (Board cells (x,y)) (xc,yc))
        else getCell (Board cells (x,y)) (xc, yc)
        | xc <- [0..x - 1], yc <- [0..y - 1]
    ] (x,y)



transformCells :: CellTransformation -> BoardDefinition -> BoardDefinition
transformCells f (Board cells (x,y)) =
    foldl (transformBoard f) (Board cells (x,y)) [(xp, yp) | xp <- [0..x - 1], yp <- [0..y - 1]]


livingCellsAround :: BoardDefinition -> (Int, Int) -> Int
livingCellsAround b p = length $ filter (\c -> c == LivingCell) $ neighbours b p

newBirth :: CellTransformation
newBirth b p = if l == 3 then LivingCell else (getCell b p)
               where l = livingCellsAround b p

survivedCell :: CellTransformation
survivedCell b p = if l == 2 || l == 3 then (getCell b p) else DeadCell
               where l = livingCellsAround b p

gameOfLifeNewBirth :: BoardDefinition -> BoardDefinition
gameOfLifeNewBirth = transformCells newBirth
gameOfLifeSurvived :: BoardDefinition -> BoardDefinition
gameOfLifeSurvived = transformCells survivedCell
gameOfLife = gameOfLifeSurvived . gameOfLifeNewBirth
