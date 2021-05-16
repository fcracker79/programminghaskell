{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
module HaskellInDepth.CH3.QuoteData where


import Data.Time (Day, parseTimeM, defaultTimeLocale)
import Data.ByteString.Char8 (unpack)
import GHC.Generics (Generic)
import Data.Csv (FromNamedRecord, FromField (..))
import Fmt(Buildable(..))

data QuoteData = QuoteData {
    day :: Day,
    volume :: Int,
    open :: Double,
    close :: Double,
    high :: Double,
    low :: Double
} deriving (Generic, FromNamedRecord)


instance FromField Day where
    parseField = parseTimeM True defaultTimeLocale "%Y-%m-%d" . unpack

{-
data QField = Open | Close | High | Low | Volume deriving (Eq, Ord, Show, Enum, Bounded)


field2fun :: QField -> QuoteData -> Double
field2fun Open = open
field2fun Close = close
field2fun High = high
field2fun Low = low
field2fun Volume = fromIntegral . volume
-}
data QField a where
    Volume :: QField Int
    Open :: QField Double
    Close :: QField Double
    High :: QField Double
    Low :: QField Double

field2fun :: QField a -> QuoteData -> a
field2fun Volume = volume
field2fun Open = open