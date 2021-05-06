{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where


import Prelude hiding(readFile, writeFile)
import HaskellInDepth.CH3.Params ( Params(..), cmdLineParser )
import Data.Csv ( decodeByName )
import Data.ByteString.Lazy (readFile)
import Data.ByteString(writeFile)
import HaskellInDepth.CH3.QuoteData ( QuoteData )
import Control.Monad (unless)
import Control.Monad.Except (when)
import HaskellInDepth.CH3.Charts ( plotChart )
import HaskellInDepth.CH3.StatReport ( statInfo, textReport )
import HaskellInDepth.CH3.HTMLReport ( htmlReport )
import Data.Text (unpack)


main :: IO ()
main = cmdLineParser >>= work


work :: Params -> IO ()
work params = do
    csvData <- readFile (fname params)
    case decodeByName csvData of
        Left err -> putStrLn err
        Right (_, quotes) -> generateReports params quotes



generateReports :: (Functor t, Foldable t) => Params -> t QuoteData -> IO ()
generateReports Params {..} quotes = do
        unless silent $ putStr textRpt
        when chart $ plotChart title quotes chartFname
        saveHtml htmlFile htmlRpt
    where
        statInfo' = statInfo quotes
        textRpt = textReport statInfo'
        htmlRpt = htmlReport title quotes statInfo' [chartFname | chart]
        withCompany prefix = maybe mempty (prefix <>) company
        chartFname = unpack $ "chart" <> withCompany "_" <> ".svg"
        title = unpack $ "Historical Quotes" <> withCompany " for "
        saveHtml Nothing _ = pure ()
        saveHtml (Just f) html = writeFile f html