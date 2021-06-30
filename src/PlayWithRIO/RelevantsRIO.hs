
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
      RIO, logInfo, HasLogFunc, runSimpleApp, liftRIO, ByteString, IsString (fromString) )
import System.IO(stdout, openFile, IOMode (ReadMode), hGetContents)
import qualified Control.Concurrent.Async.Lifted as A
import qualified UnliftIO.Async as B
import qualified Control.Concurrent.Async as C
import Prelude (putStrLn, getLine, userError, undefined, Monad (return, (>>=)), Int, drop) -- we'll explain why we need this in logging

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
{-
GHCi> :info RIO
type role RIO representational nominal
type RIO :: * -> * -> *
newtype RIO env a = RIO {unRIO :: ReaderT env IO a}
  	-- Defined in ‘rio-0.1.20.0:RIO.Prelude.RIO’
instance Applicative (RIO env)
  -- Defined in ‘rio-0.1.20.0:RIO.Prelude.RIO’
instance Functor (RIO env)
  -- Defined in ‘rio-0.1.20.0:RIO.Prelude.RIO’
instance Monad (RIO env)
  -- Defined in ‘rio-0.1.20.0:RIO.Prelude.RIO’
instance Monoid a => Monoid (RIO env a)
  -- Defined in ‘rio-0.1.20.0:RIO.Prelude.RIO’
instance Semigroup a => Semigroup (RIO env a)
  -- Defined in ‘rio-0.1.20.0:RIO.Prelude.RIO’
instance MonadIO (RIO env)
  -- Defined in ‘rio-0.1.20.0:RIO.Prelude.RIO’
instance MonadReader env (RIO env)
  -- Defined in ‘rio-0.1.20.0:RIO.Prelude.RIO’
instance MonadThrow (RIO env)
  -- Defined in ‘rio-0.1.20.0:RIO.Prelude.RIO’
instance PrimMonad (RIO env)
  -- Defined in ‘rio-0.1.20.0:RIO.Prelude.RIO’
instance MonadUnliftIO (RIO env)                   <-----------------
  -- Defined in ‘rio-0.1.20.0:RIO.Prelude.RIO’
type instance PrimState (RIO env) = PrimState IO
  	-- Defined in ‘rio-0.1.20.0:RIO.Prelude.RIO’
-}


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

--instance MonadCheers (RIO env) where
instance (Monad m, MonadIO m) => MonadCheers m where
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


myReadFile :: Int -> RIO String ByteString
myReadFile i = do
    filename <- ask
    handle <- liftIO $ openFile filename ReadMode
    contents <- liftIO $ hGetContents handle
    return $ fromString $ drop i contents

-- concurrently :: forall a b. IO a -> IO b -> IO (a, b)
{-
Couldn't match expected type ‘IO a’
              with actual type ‘RIO String ByteString’

runConcurrentlyUsingAsync = C.concurrently 
    (myReadFile 1)
    (myReadFile 2)
-}


-- MonadBaseControl IO m => m a -> m b -> m (a, b)
{-
No instance for (monad-control-1.0.2.3:Control.Monad.Trans.Control.MonadBaseControl
                     IO (RIO String))
    arising from a use of ‘A.concurrently’

runConcurrentlyUsingLiftedAsync2 = A.concurrently 
    (myReadFile 1)
    (myReadFile 2)
-}

-- concurrently :: forall (m :: * -> *) a b. MonadUnliftIO m => m a -> m b -> m (a, b)
runConcurrentlyUsingUnliftIO2 :: RIO String (ByteString, ByteString)
runConcurrentlyUsingUnliftIO2 = B.concurrently 
    (myReadFile 1)
    (myReadFile 2)