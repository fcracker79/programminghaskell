module AdventOfCode.Y2020.AOC2 where

import qualified Misc.MyParser as P
import Control.Applicative((<|>), empty, many)
import Control.Monad(guard)
import Data.Maybe(isJust)

type Password = String
data Policy = RangeLetter Int Int Char | MinLength Int deriving(Show)
data PasswordWithPolicy = PasswordWithPolicy {password :: Password, policy :: Policy} deriving(Show)

appliesPolicy :: Policy -> String -> Bool
appliesPolicy (RangeLetter a b c) s = a <= count && count <= b where count = length [x | x <- s, x == c]
appliesPolicy (MinLength a) s = length s >= a


parseRangeOfCharPolicy :: P.MyParser Policy
parseRangeOfCharPolicy = do
    i1 <- P.token P.nat
    P.symbol "-"
    i2 <- P.token P.nat
    c <- P.item
    P.symbol ":"
    return $ RangeLetter i1 i2 c

parseMinLength :: P.MyParser Policy
parseMinLength = do
    l <- P.token P.nat
    P.symbol ":"
    return $ MinLength l


allParsers :: [P.MyParser Policy]
allParsers = [parseRangeOfCharPolicy, parseMinLength]

parsePolicy :: P.MyParser Policy
parsePolicy = foldl (<|>) empty allParsers 

parsePasswordWithPolicy :: P.MyParser PasswordWithPolicy
parsePasswordWithPolicy = do
    policy <- parsePolicy
    password <- P.token (many P.item)
    return PasswordWithPolicy { password = password, policy = policy}


validPasswordWithPolicy :: PasswordWithPolicy -> Bool
validPasswordWithPolicy PasswordWithPolicy {password=password, policy=policy} = 
    appliesPolicy policy password

countValidPasswords :: [String] -> Int
countValidPasswords s = length $ do
    s' <- s
    let parsingResult = P.parse parsePasswordWithPolicy s'
    guard $ isJust parsingResult
    let Just (passwordWithPolicy, _) = parsingResult
    guard $ validPasswordWithPolicy passwordWithPolicy
    return True
