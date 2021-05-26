{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module HaskellInDepth.CH4.PaoloFolds where


import Colonnade (Colonnade, Headed, ascii, headed)
import Control.Arrow ((&&&), (<<<))
import Control.Foldl (Fold (Fold))
import qualified Control.Foldl as L
import Control.Lens (Profunctor (lmap))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Csv (FromField (..), FromRecord, HasHeader (HasHeader), ToField, ToRecord)
import Data.Csv.Streaming (Records (Cons, Nil), decode)
import Data.String (String)
import Data.Time (Day, UTCTime, defaultTimeLocale, diffDays, parseTimeM)
import Data.Time.Clock (UTCTime (UTCTime))
import Graphics.Rendering.Chart.Backend.Diagrams (toFile)
import Graphics.Rendering.Chart.Easy
    ( plot_lines_title,
      plot_lines_values,
      opaque,
      (.~),
      line_color,
      plot_lines_style,
      (&),
      gray,
      red,
      blue,
      green,
      layout_plots,
      (.=),
      layout_title,
      ToPlot(toPlot),
      PlotLines,
      Colour,
      Default(def),
      Profunctor(lmap, rmap) )
import Numeric (showFFloat)
import Protolude
    ( ($),
      fromIntegral,
      join,
      Eq((==)),
      Fractional((/)),
      Monad((>>=), (>>)),
      Functor(fmap),
      Num(abs),
      Show,
      Applicative(pure, (<*>)),
      Traversable(sequenceA, sequence),
      Generic,
      Monoid(mconcat),
      Bool(True),
      Double,
      Int,
      Integer,
      Maybe(..),
      IO,
      Either,
      (&),
      rights,
      fix,
      FilePath,
      maybeToList,
      (.),
      (<$>),
      decodeUtf8,
      Print(putStrLn),
      MonadIO(liftIO),
      ConvertText(toS),
      (>), ($>) )
import Streaming ( Stream, Of )
import qualified Streaming.Prelude as S
import Protolude.Base (Fractional)
import Control.Monad (guard)
import Data.Eq ((==))

------------ logic -------------------------------------

instance FromField Day where
  parseField r = parseTimeM True defaultTimeLocale "%F" $ toS $ decodeUtf8 r

data Row = Row
  { day :: Day
  , close :: Double
  , volume :: Integer
  , open :: Double
  , high :: Double
  , low :: Double
  }
  deriving (Generic, FromRecord, Show)

-- folding in constant space works if the state is strict
data Feature = Feature
  { feature_name :: !String
  , feature_mean :: !Double
  , feature_min :: !Double
  , feature_max :: !Double
  , feature_days :: !Integer
  }
  deriving (Show, Generic, ToRecord)


safeMean :: Fractional a => Fold a (Maybe a)
safeMean = fmap f a 
  where 
        f :: Fractional a => (a, Int) -> Maybe a
        f (n, d) = guard (d > 0) $> (n / fromIntegral d)
        a = (,) <$> L.sum <*> L.length


-- per feature Fold applicative composition
feature :: String -> (Row -> Double) -> Fold Row (Maybe Feature)
feature feature_name f = rmap ff a-- fmap
  where 
    ff = \case
      (Nothing, _, _) -> Nothing
      (Just (feature_min, dmin), Just (feature_max, dmax), Just feature_mean) -> Just $ Feature {..}
        where
          feature_days = abs $ diffDays dmin dmax
    a = (,,)
            <$> lmap (f &&& day) L.minimum
            <*> lmap (f &&& day) L.maximum
            <*> lmap f safeMean


-- per field applicative composition
reports :: Fold Row (Maybe [Feature])
reports =
  sequence
    <$> sequenceA do
      [ feature "open" open
        , feature "close" close
        , feature "high" high
        , feature "low" low
        , feature "Integer" $ fromIntegral . volume
        ]

-- caveman, forgets all problems (S.fold_ + S.concat )
runReports :: Monad m => Stream (Of (Either String Row)) m r -> m [Feature]
runReports = fmap (join . maybeToList) . L.purely S.fold_ reports . S.concat

---- rendering -----------------

colFeature :: Colonnade Headed Feature String
colFeature =
  mconcat
    [ headed "Feature" feature_name
    , headed "Mean" $ showF . feature_mean
    , headed "Min" $ showF . feature_min
    , headed "Max" $ showF . feature_max
    , headed "Days" $ showF . fromIntegral . feature_days
    ]

showF :: Double -> String
showF v = showFFloat (Just 2) v ""

-------- run ---------------------
streamCSV :: FilePath -> Stream (Of (Either String Row)) IO (Maybe (String, BL.ByteString))
streamCSV dataPath = do
  liftIO (decode HasHeader <$> BL.readFile do dataPath) >>= fix do
    \go -> \case
      Cons x r -> S.yield x >> go r
      Nil Nothing "" -> pure Nothing
      Nil (Just e) b -> pure $ Just (e, b)

dataPath :: FilePath
dataPath = "src/Chapter_3/quotes.csv"

main :: IO ()
main = runReports (streamCSV dataPath) >>= putStrLn . ascii colFeature

graph :: IO ()
graph = do
  rows <- rights <$> S.toList_ (streamCSV dataPath)
  toFile def "src/Chapter_3/quotes.svg" $ do
    layout_title .= "quote data"
    layout_plots .= fmap
      do toPlot . lineOf rows
      do
        [ ("open", open, green)
          , ("close", close, blue)
          , ("high", high, red)
          , ("low", low, gray)
          ]

lineOf :: [Row] -> (String, Row -> y, Colour Double) -> PlotLines Day y
lineOf vs (t, f, c) =
  def
    & plot_lines_style . line_color .~ opaque c
    & plot_lines_values .~ [(day &&& f) <$> vs]
    & plot_lines_title .~ t
