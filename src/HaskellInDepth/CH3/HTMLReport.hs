{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellInDepth.CH3.HTMLReport where


import Text.Blaze.Html5 as H
    ( string,
      text,
      Html,
      ToValue(toValue),
      (!),
      body,
      docTypeHtml,
      h1,
      head,
      i,
      img,
      style,
      title )
import Text.Blaze.Html5.Attributes (src)
import Fmt (Buildable, pretty)
import Colonnade ( headed, Colonnade, Headed )
import HaskellInDepth.CH3.StatReport
    ( StatEntry(qfield, meanVal, minVal, maxVal, daysBetweenMinMax),
      showPrice )
import HaskellInDepth.CH3.QuoteData
    ( QuoteData(day, open, close, high, low, volume) )
import Text.Blaze.Html.Renderer.Pretty ( renderHtml )
import Control.Monad (unless)
import qualified Data.Foldable as F
import Text.Blaze.Colonnade(encodeHtmlTable)
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (fromString)

viaFmt :: Buildable a => a -> Html
viaFmt = text . pretty


colStats :: Colonnade Headed StatEntry Html
colStats = mconcat
        [ headed "Quote Field" (i . string . show . qfield)
          , headed "Mean" (viaFmt . meanVal)
          , headed "Min" (viaFmt . minVal)
          , headed "Max" (viaFmt . maxVal)
          , headed "Days between Min/Max" (viaFmt . daysBetweenMinMax)
        ]


colData :: Colonnade Headed QuoteData Html
colData = mconcat
    [
        headed "Day" (viaFmt . day)
        , headed "Open" (viaFmt . showPrice . open)
        , headed "Close" (viaFmt . showPrice . close)
        , headed "High" (viaFmt . showPrice . high)
        , headed "Low" (viaFmt . showPrice . low)
        , headed "Volume" (viaFmt . volume)
    ]


htmlReport :: (Functor t, Foldable t) => String -> t QuoteData -> [StatEntry] -> [FilePath] -> ByteString
htmlReport docTitle quotes statEntries images = fromString $ renderHtml $ docTypeHtml $ do
    H.head $ do
        title $ string docTitle
        style tableStyle
    body $ do
        unless (F.null images) $ do
            h1 "Charts"
            F.traverse_ ((img!).src.toValue) images
        
        h1 "Statistics Report"
        encodeHtmlTable mempty colStats statEntries
        
        h1 "Stock Quotes Data"
        encodeHtmlTable mempty colData quotes
    where
        tableStyle = "table {border-collapse: collapse}" <> "td, th {border: 1px solid black; padding: 5px}"
