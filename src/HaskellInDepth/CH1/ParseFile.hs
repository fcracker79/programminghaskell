module HaskellInDepth.CH1.ParseFile where


import Data.Char ( toLower, isLetter, toLower )
import Data.List (group, sort, sortOn)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment ( getArgs )
import Control.Monad (join)


type Entry = (T.Text, Int)
type Vocabulary = [Entry]
extractVocab :: T.Text -> Vocabulary
extractVocab = map (\x -> (head x, length x)) . group . sort . map T.toCaseFold . filter (not . T.null)
              . map (T.dropAround $ not . isLetter) . T.words

-- TIO.putStrLn $ T.unwords 
printAllWords :: Vocabulary -> IO ()
printAllWords = print . show
processTextFile :: FilePath -> IO ()
processTextFile fname = do
    ws <- extractVocab <$> TIO.readFile fname
    printAllWords ws
    print $ length ws

main :: IO ()
main = getArgs >>= mconcat . fmap processTextFile


uniqueWords :: String -> [String]
uniqueWords = map head . group . sort . words . map toLower

firstUniqueWordsBy :: Ord a => ([String] -> a) -> String -> [String]
firstUniqueWordsBy f = map head . sortOn f . group . sort . words . map toLower

firstUniqueWordsFromFile :: FilePath -> IO [String]
firstUniqueWordsFromFile s = uniqueWords <$> readFile s

firstUniqueMostUsedWordsFromFile :: FilePath -> IO [String]
firstUniqueMostUsedWordsFromFile s = firstUniqueWordsBy (\x -> -length x) <$> readFile s
