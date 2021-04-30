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
module HaskellInDepth.CH1.PaolinoExercises.OrderedList(insert, remove) where

import Data.Monoid((<>))
import Data.FingerTree ( FingerTree, Measured(..), (<|), (|>), split, fromList )
import Protolude
    ( Eq,
      Num,
      Ord,
      Show,
      Int,
      Maybe(Just),
      (.),
      notImplemented,
      Last(Last, getLast),
      (>),
      Maybe(..),
      Bool(..),
      (==),
      (++),
      const,
      Typeable,
      Foldable(toList) )
import Prelude (String, show)
import Data.Typeable (cast)



type Ordered a = FingerTree (Last a) a


splitPoint :: (Ord a, Measured (Last a) a) => a -> Last a -> Bool
splitPoint x (Last Nothing) = False
splitPoint x (Last (Just y)) = y > x

splitPointEq :: (Ord a, Measured (Last a) a) => a -> Last a -> Bool
splitPointEq x (Last Nothing) = False
splitPointEq x (Last (Just y)) = y == x

dropFirst :: (Measured v a, Foldable t) => t a -> FingerTree v a
dropFirst b = fromList xs where (_:xs) = toList b

-- extract 'k' highest scoring Song , O (1) * k
insert :: (Measured (Last a) a, Ord a) => a -> Ordered a -> Ordered a
insert x xs = (left |> x) <> right
  where
    (left, right) = split (splitPoint x) xs


remove :: (Measured (Last a) a, Ord a) => a -> Ordered a -> Ordered a
remove x xs = leftNonequal <> newLeftEqual <> right
  where (left, right) = split (splitPoint x) xs
        (leftNonequal, leftEqual) = split (splitPointEq x) left
        newLeftEqual = dropFirst leftEqual
