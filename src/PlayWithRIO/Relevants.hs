
{-
From RIO documentation:

The goal of the rio library is to make it easier to adopt Haskell for writing production software. It is intended as a cross between:
    Collection of well designed, trusted libraries
    Useful Prelude replacement
    A set of best practices for writing production quality Haskell code
-}

module PlayWithRIO.Relevants where

import RIO


import Prelude (putStrLn, getLine) -- we'll explain why we need this in logging

data Env = Env {
    name1 :: TVar String,
    name2 :: TVar String,
    name :: String
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
      name2 = name2
  }
  runRIO env $ do
    sayHello
    sayGoodbye
  
  n1 <- readTVarIO name1
  putStrLn $ "New name1 " ++ n1

  n2 <- readTVarIO name2
  putStrLn $ "New name2 " ++ n2

sayHello :: (HasName a, MonadIO m, SetNames m, MonadReader a m) => m ()
sayHello = do
  env <- ask
  liftIO $ putStrLn $ "Hello, " ++ getName env
  newName1 <- liftIO getLine 
  setName1 newName1

sayGoodbye :: (HasName a, MonadIO m, SetNames m, MonadReader a m) => m ()
sayGoodbye = do
  env <- ask
  liftIO $ putStrLn $ "Goodbye, " ++ getName env
  newName2 <- liftIO getLine 
  setName2 newName2
