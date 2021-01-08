module AdventOfCode.Y2020.AOC6 where

import qualified Data.Set as Set
import qualified Misc.MyParser as P
import Control.Applicative(many, empty, some, (<|>))
import Data.Char(isAlpha)

type PositiveAnswer = Char
type PositiveAnswers = [PositiveAnswer]
type GroupPositiveAnswers = [PositiveAnswers]
type AllPositiveAnswers = [GroupPositiveAnswers]


countPositiveAnswers :: GroupPositiveAnswers -> Int
countPositiveAnswers g = length . mconcat $ fmap Set.fromList g

countAllPositiveAnswers :: AllPositiveAnswers -> Int
countAllPositiveAnswers g = sum $ fmap countPositiveAnswers g


parsePositiveAnswers :: P.MyParser PositiveAnswers
parsePositiveAnswers = some $ P.sat isAlpha

anyCarriageReturn = many $ P.char '\n'

parseGroupPositiveAnswers :: P.MyParser GroupPositiveAnswers
parseGroupPositiveAnswers = do
    x <- parsePositiveAnswers
    cr <- anyCarriageReturn

    case length cr of
        1 -> do
            xs <- parseGroupPositiveAnswers
            return (x: xs)
        _ -> return [x]


parseAllPositiveAnswers :: P.MyParser AllPositiveAnswers
parseAllPositiveAnswers = many parseGroupPositiveAnswers
