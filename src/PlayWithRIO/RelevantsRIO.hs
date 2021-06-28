
{-
From RIO documentation:

The goal of the rio library is to make it easier to adopt Haskell for writing production software. It is intended as a cross between:
    Collection of well designed, trusted libraries
    Useful Prelude replacement
    A set of best practices for writing production quality Haskell code
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module PlayWithRIO.RelevantsRIO where

import RIO
    ( (++),
      ($),
      Eq((==)),
      Monad,
      IO,
      String,
      MonadReader(ask),
      const,
      when,
      runRIO,
      modifyTVar',
      throwIO,
      atomically,
      newTVarIO,
      readTVarIO,
      MonadIO(..),
      TVar,
      Handle,
      RIO, logInfo, HasLogFunc, runSimpleApp, liftRIO )
import System.IO(stdout)

import Prelude (putStrLn, getLine, userError, undefined, Monad (return, (>>=))) -- we'll explain why we need this in logging

data Env = Env {
    name1 :: TVar String,
    name2 :: TVar String,
    name :: String,
    handle :: Handle 
}

class HasName a where
    getName :: a -> String

class Monad m => SetNames m where
    setName1 :: String -> m ()
    setName2 :: String -> m ()

instance HasName Env where
    getName = name


instance SetNames (RIO Env) where
    setName1 newName1 = do
        env <- ask
        atomically $ modifyTVar' (name1 env) $ const newName1
    setName2 newName2 = do
        env <- ask
        atomically $ modifyTVar' (name2 env) $ const newName2

-- newtype RIO env a = RIO {unRIO :: ReaderT env IO a}
-- https://www.fpcomplete.com/blog/2017/06/readert-design-pattern/
readerPatternApplied :: IO ()
readerPatternApplied = do
  name1 <- newTVarIO ""
  name2 <- newTVarIO ""
  let env = Env {
      name = "Alice",
      name1 = name1,
      name2 = name2,
      handle = stdout
  }
  runRIO env $ do
    sayHello
    sayGoodbye
  
  n1 <- readTVarIO name1
  
  putStrLn $ "New name1 " ++ n1

  n2 <- readTVarIO name2
  putStrLn $ "New name2 " ++ n2


class Monad m => MonadCheers m where
    hello :: String -> m ()
    goodbye :: String -> m ()
    askForName :: m String

instance MonadCheers (RIO env) where
    hello s = liftIO $ putStrLn $ "Hello, " ++ s
    goodbye s = liftIO $ putStrLn $ "Goodbye, " ++ s
    askForName = do
        name <- liftIO getLine
        when (name == "") $ throwIO $ userError "Empty name"
        return name


sayHello :: (HasName a, SetNames m, MonadReader a m, MonadCheers m) => m ()
sayHello = do
  env <- ask
  hello $ getName env
  askForName >>= setName1


sayGoodbye :: (HasName a, SetNames m, MonadReader a m, MonadCheers m) => m ()
sayGoodbye = do
  env <- ask
  goodbye $ getName env
  askForName >>= setName2


playWithLiftIO :: IO ()
playWithLiftIO = do
    runSimpleApp sayHelloLiftIO
    runSimpleApp sayHelloRIOLiftIO


sayHelloRIOLiftIO :: HasLogFunc env => RIO env ()
sayHelloRIOLiftIO = logInfo "Hello World!"

sayHelloLiftIO :: (MonadReader env m, MonadIO m, HasLogFunc env) => m ()
sayHelloLiftIO = liftRIO sayHelloRIOLiftIO
