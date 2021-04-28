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

import Data.FingerTree ( FingerTree, Measured(..) )
import Protolude
    ( Eq,
      Num,
      Ord,
      Show,
      Int,
      Maybe(Just),
      (.),
      notImplemented,
      Last(Last) )



type Ordered a = FingerTree (Last a) a

-- extract 'k' highest scoring Song , O (1) * k
insert :: (Measured (Last a) a, Ord a) => a -> Ordered a -> Ordered a
insert x xs = notImplemented 

remove :: (Measured (Last a) a, Ord a) => a -> Ordered a -> Ordered a
remove x xs = notImplemented 
