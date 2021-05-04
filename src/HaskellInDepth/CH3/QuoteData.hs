module HaskellInDepth.CH3.QuoteData where


import Data.Time(Day)


data QuoteData = QuoteData {
    day :: Day,
    volume :: Int,
    open :: Double,
    close :: Double,
    high :: Double,
    low :: Double
}
