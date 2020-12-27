module BookOfMonads.Chapter11.JSONFileSearch 
    (searchJSONElement)
    where


import qualified Control.Monad.Trans.Maybe as M
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.IO.Class as MIO
import Control.Monad.Reader.Class
import qualified Control.Monad.Trans.State as S
import qualified Data.Aeson as J
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T


mRecLookup :: [String] -> Maybe J.Value -> Maybe J.Value
mRecLookup [] _ = Nothing
mRecLookup _ Nothing = Nothing
mRecLookup (s : sw) (Just (J.Object v))
  | null sw = HM.lookup (T.pack s) v
  | otherwise = case HM.lookup (T.pack s) v of
        Just x -> mRecLookup sw (Just x)
        _ -> Nothing



data JSONSearchCriteria = JSONSearchCriteria { jsonPath :: String, fileName :: String }

split   :: String -> Char -> [String]
split s c =  case dropWhile (== c) s of
                      "" -> []
                      s' -> w : split s'' c
                            where (w, s'') = break (== c) s'

recLookup :: String -> J.Value -> Maybe J.Value
recLookup s o = mRecLookup (split s '.') (Just o)


-- Where is withReader ?
searchJSONElement :: R.ReaderT JSONSearchCriteria (M.MaybeT IO) String
searchJSONElement = do
    searchCriteria <- ask
    let _jsonPath = jsonPath searchCriteria
    let _fileName = fileName searchCriteria

    return ""
