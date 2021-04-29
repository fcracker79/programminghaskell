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
import Data.FingerTree ( FingerTree, Measured(..), (<|), (|>), split )
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
      Bool(..) )



type Ordered a = FingerTree (Last a) a


splitPoint :: (Ord a, Measured (Last a) a) => a -> Last a -> Bool 
splitPoint x (Last Nothing) = False 
splitPoint x (Last (Just y)) = y > x

-- extract 'k' highest scoring Song , O (1) * k
insert :: (Measured (Last a) a, Ord a) => a -> Ordered a -> Ordered a
insert x xs = (left |> x) <> right 
  where 
    (left, right) = split f xs
    f = splitPoint x
remove :: (Measured (Last a) a, Ord a) => a -> Ordered a -> Ordered a
remove x xs = notImplemented 
