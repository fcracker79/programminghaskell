{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module HaskellInDepth.CH5.Relevants where

import Protolude
    ( ($),
      Monad(return),
      MonadFail,
      Char,
      Int,
      Maybe(Just),
      IO,
      State,
      lift,
      MonadReader(ask),
      print, Applicative, MonadIO, Functor, MonadState (get), Sum(..) )

import Control.Monad.State ( State )
import Control.Monad.Writer (MonadWriter, WriterT, tell )
import Control.Monad.Reader
    ( Monad(return),
      Functor,
      MonadFail,
      MonadIO,
      MonadReader(ask),
      MonadTrans(lift),
      ReaderT(ReaderT) )

newtype MyRWS r w s a = MyRWS (ReaderT r (WriterT w (State s)) a)
    deriving (Functor, Applicative, Monad,
                MonadReader r,
                MonadWriter w,
                MonadState s)

dino :: MyRWS Int (Sum Int) [Char] ()
dino = do
    -- Reader
    x <- ask
    -- Writer
    tell $ Sum 1
    -- State
    s <- get
    return ()


mayFail :: (MonadFail m) => m [Int] -> m Int
mayFail mm = do
    (x: xs) <- mm
    return x

-- Output:
-- Just 1
-- Nothing
main :: IO ()
main = do
    let x = Just [1]
    print $ mayFail x
    let x = Just []
    print $ mayFail x
    
    return ()
