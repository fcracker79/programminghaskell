{-# LANGUAGE FlexibleContexts #-}


module PlayWithRIO.ReaderTWithIOExample where

import Control.Concurrent.Async.Lifted.Safe ( concurrently )
import Control.Monad.Reader
    ( MonadIO(..), MonadReader(ask), ReaderT(runReaderT) )
import Control.Concurrent.STM
    ( atomically, newTVarIO, readTVarIO, modifyTVar', TVar, newTVar )

modify :: (MonadReader (TVar Int) m, MonadIO m)
       => (Int -> Int)
       -> m ()
modify f = do
  ref <- ask
  liftIO $ atomically $ modifyTVar' ref f

main :: IO ()
main = do
  ref <- newTVarIO 4
  runReaderT (concurrently (modify (+ 1)) (modify (+ 2))) ref
  readTVarIO ref >>= print
