module HaskellInDepth.CH1.PaolinoExercises.Vocabulary where


import Data.Text(Text)
import Data.Set(Set, fromList, elems, member, insert)
import Data.List (sort, group)
import Control.Monad.State (evalState, State, put, get)
import qualified Data.Map as M
import Data.Foldable (foldl')
import Data.Function ((&))

-- as in the book
uniqueWords :: [Text] -> [Text]
uniqueWords = map head . group . sort

-- use Set internally 
uniqueWordsSet :: [Text] -> [Text]
uniqueWordsSet = elems . fromList

-- keep order of appearance , do not use Set
uniqueWithOrder :: [Text] -> [Text]
uniqueWithOrder [] = []
uniqueWithOrder (x:xs) = if x `elem` xs then uxs else x:uxs where uxs = uniqueWithOrder xs

-- keep appearence order , use Set ,no Monad State 
uniqueWithOrderSet :: [Text] -> [Text]
uniqueWithOrderSet = f mempty
    where f s [] = []
          f s (x:xs) = if x `member` s then f s xs else x:f newset xs
                       where newset = insert x s

-- keep appearence order , use Set , use Monad State 
uniqueWithOrderSetState :: [Text] -> [Text]
uniqueWithOrderSetState s = evalState (f s) mempty
    where f :: [Text] -> State (Set Text) [Text]
          f [] = return []
          f (x:xs) = do
              currentElements <- get
              put $ insert x currentElements
              if x `elem` currentElements then f xs else (x:) <$> f xs


type Vocabulary = [(Text, Int)]
extractVocabMapBrutto  :: [Text] -> Vocabulary
extractVocabMapBrutto xs = M.assocs $ foldl' f mempty xs
    where 
        f :: M.Map Text Int -> Text -> M.Map Text Int
        f b a = M.unionWith (+) b (M.singleton a 1)

-- TODO understand what I did
extractVocabMap  :: [Text] -> Vocabulary
extractVocabMap xs = M.assocs $ foldl' (&) mempty v
    where v = fmap (\x m -> M.unionWith (+) m (M.singleton x 1)) xs
