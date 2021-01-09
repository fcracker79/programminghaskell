module BookOfMonads.Chapter11.JSONFileSearch 
    (
        searchJSONElement,
        JSONOrigin(..),
        JSONSearchCriteria(..)
    )
    where


import qualified Control.Monad as CM
import qualified Control.Monad.Trans.Class as MTC
import qualified Control.Monad.Trans.Maybe as M
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.IO.Class as MIO
import Control.Monad.Reader.Class
import qualified Control.Monad.Trans.State as S
import qualified Data.Aeson as J
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as L
import AdventOfCode.Utils(split)

mRecLookup :: [String] -> Maybe J.Value -> Maybe J.Value
mRecLookup [] _ = Nothing
mRecLookup _ Nothing = Nothing
mRecLookup (s : sw) (Just (J.Object v))
  | null sw = HM.lookup (T.pack s) v
  | otherwise = case HM.lookup (T.pack s) v of
        Just x -> mRecLookup sw (Just x)
        _ -> Nothing


data JSONOrigin = FilePath String | JSONContent L.ByteString


data JSONSearchCriteria = JSONSearchCriteria { jsonOrigin :: JSONOrigin, jsonPath :: String }

getJSONContent :: R.ReaderT JSONOrigin (M.MaybeT IO) L.ByteString
getJSONContent = do
    criteria <- ask
    case criteria of
        JSONContent s -> MTC.lift . M.MaybeT . pure . Just $ s
        FilePath p -> MIO.liftIO . L.readFile $ p

searchJSONElement :: R.ReaderT JSONSearchCriteria (M.MaybeT IO) J.Value
searchJSONElement = do
    searchCriteria <- ask
    let _jsonPath = (`split` '.') . jsonPath $ searchCriteria
    jsonContents <- J.decode <$> R.withReaderT jsonOrigin getJSONContent
    MTC.lift . M.MaybeT . pure $ mRecLookup _jsonPath jsonContents
