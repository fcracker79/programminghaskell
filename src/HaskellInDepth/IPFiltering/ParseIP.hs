module HaskellInDepth.IPFiltering.ParseIP where


import HaskellInDepth.IPFiltering.IPTypes ( IP(IP), IPRange (IPRange), ParseError (ParseError), IPRangeDB (IPRangeDB) )
import Control.Applicative (Alternative (empty))
import Data.Word (Word8)
import Control.Monad ( (>=>) )
import Text.Read (readMaybe)
import Data.List.Extra ( splitOn )


guarded :: Alternative f => (a -> Bool) -> a -> f a
guarded f a = if f a then pure a else empty


isLengthOf :: Int -> [a] -> Bool
isLengthOf n xs = length xs == n


buildIP :: [Word8] -> IP
buildIP = IP . fst . foldr go (0, 1)
    where go b (s, k) = (s + fromIntegral b * k, k*256)

parseIP :: String -> Maybe IP
parseIP = guarded (4 `isLengthOf`) . splitOn "." >=> mapM (readMaybe >=> guarded fitsOctet) >=> pure . buildIP
    where fitsOctet x = 0 <= x && x <= 255

parseIPRange :: String -> Maybe IPRange
parseIPRange = guarded (2 `isLengthOf`) . splitOn "," >=> mapM parseIP >=> listToIPRange
    where
        listToIPRange [a,b] = pure (IPRange a b)
        listToIPRange _ = empty

parseIPRanges :: String -> Either ParseError IPRangeDB
parseIPRanges = fmap IPRangeDB . mapM parseLine . zip [1..] . lines
    where
        parseLine (ln, s) = case parseIPRange s of
            Nothing -> Left (ParseError ln)
            Just ipr -> Right ipr
