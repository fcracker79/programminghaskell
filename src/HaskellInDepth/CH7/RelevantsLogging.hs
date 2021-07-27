{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellInDepth.CH7.RelevantsLogging where

import Data.Text ( pack )
import Control.Monad.Trans ( MonadTrans(lift) )
import Control.Monad.Trans.State ( StateT (runStateT), get, put )
import Control.Monad.Logger ( logDebug, LoggingT, runStdoutLoggingT, logInfoN, runFileLoggingT )


popAndLog :: LoggingT (StateT [Int] IO) ()
popAndLog = do
  _:xs <- lift get
  lift (put xs)
  $logDebug ("***" <> pack (show xs) <> "***")
  -- This does not provide loc info in the logging message
  logInfoN "This is from logInfoN"


{-
[Debug] ***[2,3]*** @(main:HaskellInDepth.CH7.RelevantsLogging /home/mirko/dev/haskell/programminghaskell/src/HaskellInDepth/CH7/RelevantsLogging.hs:16:3)
((),[2,3])
-}
main :: IO ((), [Int])
main = runStateT (runStdoutLoggingT popAndLog) [1,2,3]


-- logging to file
mainFile :: IO ((), [Int])
mainFile = runStateT (runFileLoggingT "/tmp/log.txt" popAndLog) [1,2,3]