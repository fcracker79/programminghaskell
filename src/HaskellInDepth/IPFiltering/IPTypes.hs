{-# LANGUAGE DeriveAnyClass #-}
module HaskellInDepth.IPFiltering.IPTypes where

import Data.Word (Word32)
import Control.Monad.Catch ( Exception )
newtype IP = IP {unIP :: Word32} deriving (Eq, Ord, Show)
data IPRange = IPRange IP IP deriving Eq
newtype IPRangeDB = IPRangeDB [IPRange] deriving Eq


type LineNumber = Int

--The type of exceptions that resulted from analyzing user input
newtype ParseError = ParseError LineNumber deriving (Show, Eq)
data InvalidArgsException = LoadIPRangesError ParseError | InvalidIP String deriving Exception
instance Show InvalidArgsException where
    show (LoadIPRangesError (ParseError idx)) = "Error loading ip range databases (line: " ++ show idx ++ ")"
    show (InvalidIP s) = "Invalid IP address to check: " ++ s