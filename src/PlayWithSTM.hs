{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PlayWithSTM where

import Control.Concurrent.STM
import Control.Monad
import Debug.Trace(trace)
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad.State


data Item
  = Scroll
  | Wand
  | Banjo
  deriving (Eq, Ord, Show)

newtype Gold = Gold Int
  deriving (Eq, Ord, Show, Num)

newtype HitPoint = HitPoint Int
  deriving (Eq, Ord, Show, Num)

type Inventory = TVar [Item]

type Health = TVar HitPoint

type Balance = TVar Gold

data Player = Player
  { balance :: Balance,
    health :: Health,
    inventory :: Inventory
  }

basicTransfer :: Num a => a -> TVar a -> TVar a -> STM ()
basicTransfer qty fromBal toBal = do
  fromQty <- readTVar fromBal
  toQty <- readTVar toBal
  writeTVar fromBal (fromQty - qty)
  writeTVar toBal (toQty + qty)


data MyState = MyState { semaphores :: [MVar ()], gold :: [TVar Gold] }

sprint :: String -> StateT MyState IO ()
sprint = liftIO . print

main :: IO ()
main = do
  (alice, bob) <- evalStateT go emptyState
  print $ "Alice has " ++ show alice ++ " gold, whereas Bob has " ++ show bob ++ " gold"

  where 
    emptyState :: MyState
    emptyState = MyState { semaphores=[], gold=[]}

go :: StateT MyState IO (Gold, Gold)
go = do
  sprint "Creating semaphores"
  semaphoreAlice <- liftIO newEmptyMVar
  semaphoreBob <- liftIO newEmptyMVar
  sprint "Creating STM variables"
  (alice, bob) <- liftIO createVariables
  sprint "Storing state"
  put $ MyState { semaphores=[semaphoreAlice, semaphoreBob], gold=[alice, bob]}

  sprint "Starting thread 1"
  t1 <- goThread 0 3
  sprint "Starting thread 2"
  t2 <- goThread 1 5

  sprint "Waiting for Alice"
  liftIO $ readMVar semaphoreAlice
  sprint "Waiting for Bob"
  liftIO $ readMVar semaphoreBob
  
  newAlice <- (liftIO . readTVarIO) alice
  newBob <- (liftIO . readTVarIO) bob
  return (newAlice, newBob)

  where 
    goThread :: Int -> Gold -> StateT MyState IO ()
    goThread i q = do
        sprint $ "Increasing " ++ show i ++ " of quantity " ++ show q
        s <- get
        let to = gold s !! i
        let from = gold s !! mod (i + 1) 2
        (liftIO . atomically) $ basicTransfer q from to
        liftIO $ putMVar (semaphores s !! i) ()
        sprint $ "Finished with " ++ show i
    createVariables :: IO (TVar Gold, TVar Gold)
    createVariables = atomically $ do
      alice <- newTVar (100 :: Gold)
      bob <- newTVar 200
      return (alice, bob)
