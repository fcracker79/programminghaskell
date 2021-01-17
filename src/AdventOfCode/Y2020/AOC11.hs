module AdventOfCode.Y2020.AOC11 where 


import Debug.Trace(trace)


data Seats = Seats { width :: Int, height :: Int, seats :: String } deriving(Eq)


secSeats :: Int -> String -> String
secSeats w [] = ""
secSeats w s = show (take w s) ++ "\n" ++ secSeats w (drop w s)

instance Show Seats where
    show s = secSeats (width s) (seats s)


getSeat :: (Int, Int) -> Seats -> Char
getSeat (x, y) s
    | x < 0 || x >= width s = 'X'
    | y < 0 || y >= height s = 'X'
    | otherwise = seats s !! (x + y * width s)


adiacentPositions :: (Int, Int) -> [(Int, Int)]
adiacentPositions (x, y) = [(x + xi, y + yi) | xi <- [-1, 0, 1], yi <- [-1, 0, 1], xi /= 0 || yi /= 0]

notAdiacentOccupied :: (Int, Int) -> Seats -> Bool
notAdiacentOccupied g s = notElem '#' $ fmap (`getSeat` s) (adiacentPositions g)

fourOrMoreAdiacentOccupied :: (Int, Int) -> Seats -> Bool
fourOrMoreAdiacentOccupied g s = length (filter (== '#') $ fmap (`getSeat` s) (adiacentPositions g)) >= 4

seatTurn :: (Int, Int) -> Seats -> Char
seatTurn g s
    | curSeat == 'L' && notAdiacentOccupied g s = '#'
    | curSeat == '#' && fourOrMoreAdiacentOccupied g s = 'L'
    | otherwise = curSeat
    where curSeat = getSeat g s

seatsTurn :: Seats -> Seats
seatsTurn s = Seats {
        width = width s,
        height = height s,
        seats = [seatTurn (xp, yp) s | yp <- [0..(height s - 1)], xp <- [0..(width s - 1)]]
    }


turnsCount :: Seats -> Int
turnsCount s
    | s == newSeats = length $ filter (== '#') (seats s)
    | otherwise = turnsCount (trace (show newSeats) newSeats)
    where newSeats = seatsTurn s


createSeats :: Int -> String -> Seats
createSeats width seats = Seats {
        seats = seats,
        width = width,
        height = length seats `quot` width    
    }