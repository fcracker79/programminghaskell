
{-# LANGUAGE StandaloneDeriving #-}
module HaskellInDepth.CH2.ShowAndRead where

import Data.String (IsString(..))
import Fmt
data Person = Person String (Maybe Int)
deriving instance Show Person
deriving instance Read Person
deriving instance Eq Person

instance IsString Person where
    fromString name = Person name Nothing
