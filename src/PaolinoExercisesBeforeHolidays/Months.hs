{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall #-}

module PaolinoExercisesBeforeHolidays.Months where


import Protolude
import qualified Data.Map as M
import qualified Data.Set as S

-- states that what is alive from day start until day end or forever in case of Nothing
data Row d a = Row
  { -- | first day the 'a' was in 
    enter :: d
  , -- | last day the 'a' was in, Nothing means never exit
    exit :: Maybe d
  , -- | anything we want as long as it can go in a Set
    what :: a
  }

-- it's always possible to extract the month from a day
type MonthOf d t = d -> t



data Direction = In | Out deriving(Show)
type ElementWithDirection d a = (d, a, Direction)
dayOf :: ElementWithDirection d a -> d
dayOf (d, _, _) = d

elementOf :: ElementWithDirection d a -> a
elementOf (_, a, _) = a

directionOf :: ElementWithDirection d a -> Direction
directionOf (_, _, d) = d


elementWithDirection :: (Enum d, Ord d) => d -> Row d a -> (ElementWithDirection d a, ElementWithDirection d a)
elementWithDirection lastDay r = (start, end)
  where 
    start = (enter r, element, In)
    end = (succ (maybe lastDay (min lastDay) (exit r)), element, Out)
    element = what r

elementsWithDirection :: (Ord d, Enum d) => d -> [Row d a] -> [ElementWithDirection d a]
elementsWithDirection _ [] = []
elementsWithDirection lastDay (h:t) = fst he:snd he:elementsWithDirection lastDay t
  where he = elementWithDirection lastDay h

sortElementsWithDirection :: Ord d => [ElementWithDirection d a] -> [ElementWithDirection d a]
sortElementsWithDirection = sortBy (\ a b -> compare (dayOf a) (dayOf b))

data AccumulationState t d a = AccumulationState { 
  currentMonth :: t, 
  curSet :: Set a,
  instancesCount :: Map a Int
}

safeJust :: a -> (a -> a) -> Maybe a -> Maybe a
safeJust _ f (Just x) = (Just . f) x
safeJust x f Nothing = (Just . f) x


updateAccumulationState :: Ord a => AccumulationState t d a -> ElementWithDirection d a -> AccumulationState t d a
updateAccumulationState s (_, a, In) = AccumulationState {
  currentMonth = currentMonth s,
  curSet = S.insert a (curSet s),
  instancesCount = M.alter (safeJust 0 (+1)) a (instancesCount s)
}
updateAccumulationState s (_, a, Out) = AccumulationState {
  currentMonth = currentMonth s,
  curSet = if fromMaybe 0 (M.lookup a newInstancesCount) == 0 then S.delete a (curSet s) else curSet s,
  instancesCount = newInstancesCount
}
  where newInstancesCount = M.alter (safeJust 1 (\x -> x - 1)) a (instancesCount s)

newAccumulationState :: MonthOf d t -> AccumulationState t d a -> ElementWithDirection d a -> AccumulationState t d a
newAccumulationState monthOf oldState (d, _, _) = AccumulationState {
  currentMonth = monthOf d,
  curSet = curSet oldState,
  instancesCount = instancesCount oldState
}

months2 :: (Eq t, Enum t, Ord a) => MonthOf d t -> AccumulationState t d a -> [ElementWithDirection d a] -> [(t, Set a)]
months2 _ s [] = [(currentMonth s, curSet s)]
months2 monthOf s (h:t)
  | monthOf (dayOf h) == currentMonth s = months2 monthOf (updateAccumulationState s h) t
  | otherwise = fmap (, lastSet) (ord2array (currentMonth s) (monthOf (dayOf h))) ++ remainingStuff
  where 
    lastSet = curSet s
    remainingStuff = months2 monthOf (updateAccumulationState (newAccumulationState monthOf s h) h) t

ord2array :: (Enum a, Eq a) => a -> a -> [a]
ord2array a b = if a == b then [] else a:ord2array (succ a) b

myWhatWasThere
  :: (Eq t, Ord a, Ord d, Enum d, Enum t)
  => MonthOf d t -- ^ how to compute the month of the day
  -> d -- ^ last day of the report , any ingress after this day should be not reported
  -> [Row d a] -- ^ input, increasing in 'd'
  -> [(t, Set a)] -- ^ increasing 'm', all 'a' alive in the relative month
myWhatWasThere _ _ [] = []
myWhatWasThere monthOf lastDay (h:t) = months2 monthOf s es
  where 
    s = AccumulationState {
      currentMonth = monthOf (enter h),
      curSet = S.empty,
      instancesCount = M.empty
    }
    es = sortElementsWithDirection $ elementsWithDirection lastDay (h:t)


-- compute the presence of the 'a' in every month,
-- we expect ALL months from the first day in the rows to be there until month of lastDay
-- pls consider to produce the results online with reading the input, once you have the data for a month it should be produced
whatWasThere
  :: (Eq t, Ord a, Ord d, Enum d, Enum t)
  => MonthOf d t -- ^ how to compute the month of the day
  -> d -- ^ last day of the report , any ingress after this day should be not reported
  -> [Row d a] -- ^ input, increasing in 'd'
  -> [(t, Set a)] -- ^ increasing 'm', all 'a' alive in the relative month
whatWasThere monthOf lastDay = notImplemented


newtype Month = Month Int deriving (Eq, Ord, Show, Enum, Num)
dumbMonthOf :: Int -> Month
dumbMonthOf = succ . Month . (`div` 2) . pred


main :: IO ()
main = do
  print $ myWhatWasThere dumbMonthOf 7 [ Row 1 (Just 8) 1, Row 2 (Just 4) 2, Row 4 (Just 12) 4]
  print $ myWhatWasThere dumbMonthOf 20 [ Row 1 (Just 8) 1, Row 2 (Just 4) 2, Row 4 (Just 12) 4]
