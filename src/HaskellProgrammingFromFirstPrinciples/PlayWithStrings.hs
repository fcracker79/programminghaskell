{-# LANGUAGE OverloadedStrings #-}

module HaskellProgrammingFromFirstPrinciples.PlayWithStrings where


import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Codec.Compression.GZip as GZip

input :: BL.ByteString
input = "123"

inputStrict :: BS.ByteString 
inputStrict = "123"


compressed :: BL.ByteString
compressed = GZip.compress input

compressedStrict :: BS.ByteString
compressedStrict = BL.toStrict $ GZip.compress input

main :: IO ()
main = do
    TIO.putStrLn $ TE.decodeUtf8 (s input)
    TIO.putStrLn $ TE.decodeUtf8 inputStrict
    TIO.putStrLn $ TE.decodeUtf8 (s compressed)
    TIO.putStrLn $ TE.decodeUtf8 compressedStrict
    where s = BL.toStrict
