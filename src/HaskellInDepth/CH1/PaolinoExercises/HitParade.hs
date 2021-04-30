{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module HaskellInDepth.CH1.PaolinoExercises.HitParade where

import Data.FingerTree
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Semigroup (Max (Max))
import Protolude
    ( ($),
      Bounded,
      Eq,
      Num ((+)),
      Ord,
      Show,
      IsString,
      Foldable(toList),
      Monoid(mempty),
      Int,
      Maybe(Just, Nothing),
      notImplemented,
      (.),
      Last(Last),
      take,
      Down(..),
      Map,
      Text, Semigroup ((<>)) )
import Protolude.Base (Bool)
import Prelude (Bool(False))
import Protolude (Ord((>)))

-- how many times a song has been voted
newtype Score = Score Int
  deriving (Ord, Eq, Show, Bounded, Num)

-- song name
newtype Song = Song Text deriving (Eq, Ord, Show, IsString)

-- a song together with its score
data Hit = Hit
  { song :: Song
  , score :: Score
  }
  deriving (Eq, Show)

-- Hit ordering
type SortHit = (Down Song, Score)

-- project Hit into it's natural ordering
sortHit :: Hit -> SortHit
sortHit (Hit song score) = (Down song, score)

-- open and close Last

-- how to measure a Hit in a fingertree sense, checkout the measure must be a monoid
instance Measured (Last SortHit) Hit where
  measure (Just . sortHit -> h) = Last h

-- a fingertree where the monoid is an Last
type Ordered b a = FingerTree (Last b) a

-- song scores together with a fingertree for logarithmic time operations
data HitParade = HitParade
  { scores :: Map Song Score
  , parade :: Ordered SortHit Hit
  }
  deriving (Show)

noParade :: HitParade
noParade = HitParade mempty mempty

dropFirst :: (Measured v a, Foldable t) => t a -> FingerTree v a
dropFirst b = fromList xs where (_:xs) = toList b

splitPoint :: Hit -> Last SortHit -> Bool 
splitPoint = notImplemented 

splitPointEq :: Hit -> Last SortHit -> Bool 
splitPointEq = notImplemented 

insert :: Hit -> Ordered SortHit Hit -> Ordered SortHit Hit
insert x xs = (left |> x) <> right
    where (left, right) = split (splitPoint x) xs


remove :: Hit -> Ordered SortHit Hit -> Ordered SortHit Hit
remove x xs = leftNonequal <> newLeftEqual <> right
    where (left, right) = split (splitPoint x) xs
          (leftNonequal, leftEqual) = split (splitPointEq x) left
          newLeftEqual = dropFirst leftEqual

-- here is the plan where  n == number of songs
-- 1) get the song score , O (log n)
-- 2) remove the Hit (song + score) from the fingertree, O(log n)
-- 3) update the hit score  and insert it back into the fingertree, O(log n)
-- 4) update the scores , O (log n)
addVote :: Song -> HitParade -> HitParade
addVote song h@HitParade {..}  = case maybeSongScore of
    Nothing -> h
    Just songScore -> notImplemented
    where maybeSongScore = M.lookup song scores
          paradeWithoutHit songScore = remove (Hit song songScore) parade
          paradeWithUpdatedHit songScore = insert (Hit song (songScore + 1)) $ paradeWithoutHit songScore
          newScores songScore = M.insert song (songScore + 1) scores
          newHitParade songScore = HitParade { scores=newScores songScore, parade=paradeWithUpdatedHit songScore }

-- extract 'k' highest scoring Song , O (1) * k
hits :: Int -> HitParade -> [Hit]
hits n (HitParade _ parade) = take n $ toList parade
