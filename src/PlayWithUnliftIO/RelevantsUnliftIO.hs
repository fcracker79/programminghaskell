module PlayWithUnliftIO.RelevantsUnliftIO where


import Data.ByteString ( ByteString )
import Control.Monad.Reader ( MonadIO(liftIO), ReaderT )
import Control.Monad.IO.Unlift ( MonadIO(liftIO), MonadUnliftIO(..) )
import GHC.IO.IOMode ( IOMode (ReadMode) )
import GHC.IO.Handle ( Handle, hGetContents )
import System.IO ( withBinaryFile )
import qualified Control.Concurrent.Async.Lifted as A
import qualified UnliftIO.Async as B
import qualified Control.Concurrent.Async as C
import Control.Applicative ( Alternative(empty, (<|>)) )
import Control.Monad.Trans.Control
import GHC.Base
data Env

-- monad-control A.concurrently :: forall (m :: * -> *) a b. MonadBaseControl IO m => m a -> m b -> m (a, b)
-- unliftio      B.concurrently :: forall (m :: * -> *) a b. MonadUnliftIO m => m a -> m b -> m (a, b)
-- async         C.concurrently :: forall a b. IO a -> IO b -> IO (a, b)

{-
class transformers-base-0.4.5.2:Control.Monad.Base.MonadBase b m =>
      MonadBaseControl b m | m -> b where
  type StM :: (* -> *) -> * -> *
  type family StM m a
  liftBaseWith :: (RunInBase m b -> b a) -> m a
  restoreM :: StM m a -> m a
  {-# MINIMAL liftBaseWith, restoreM #-}
-}

-- class MonadIO m => MonadUnliftIO m where
--     withRunInIO :: ((forall a. m a -> IO a) -> IO b) -> m b


myReadFile :: FilePath -> IO ByteString
myReadFile = undefined

myReadFileWithReader :: FilePath -> ReaderT Env IO ByteString
-- myReadFileWithReader fp = ReaderT $ \_env -> myReadFile fp
myReadFileWithReader = liftIO . myReadFile



myWithBinaryFile
    :: MonadUnliftIO m
    => FilePath
    -> IOMode
    -> (Handle -> m a)
    -> m a
myWithBinaryFile fp mode inner =
  withRunInIO $ \runInIO ->
  withBinaryFile
    fp
    mode
    (runInIO . inner)

-- monad-control:Control.Monad.Trans.Control.MonadBaseControl and 
-- unliftio-core:Control.Monad.IO.Unlift.MonadUnliftIO are both implemented by IO

-- MonadBaseControl IO m => m a -> m b -> m (a, b)
runConcurrentlyUsingLiftedAsync :: IO (String, String)
runConcurrentlyUsingLiftedAsync = A.concurrently 
    (myWithBinaryFile "/tmp/dino1" ReadMode hGetContents)
    (myWithBinaryFile "/tmp/dino2" ReadMode hGetContents)

-- concurrently :: forall (m :: * -> *) a b. MonadUnliftIO m => m a -> m b -> m (a, b)
runConcurrentlyUsingUnliftIO :: IO (String, String)
runConcurrentlyUsingUnliftIO = B.concurrently 
    (myWithBinaryFile "/tmp/dino1" ReadMode hGetContents)
    (myWithBinaryFile "/tmp/dino2" ReadMode hGetContents)

-- concurrently :: forall a b. IO a -> IO b -> IO (a, b)
runConcurrentlyUsingAsync :: IO (String, String)
runConcurrentlyUsingAsync = C.concurrently 
    (myWithBinaryFile "/tmp/dino1" ReadMode hGetContents)
    (myWithBinaryFile "/tmp/dino2" ReadMode hGetContents)

-- MonadBaseControl IO m => m a -> m b -> m (a, b)
runConcurrentlyUsingLiftedAsync2 :: ReaderT Env IO (ByteString, ByteString)
runConcurrentlyUsingLiftedAsync2 = A.concurrently 
    (myReadFileWithReader "/tmp/dino1")
    (myReadFileWithReader "/tmp/dino2")


-- concurrently :: forall (m :: * -> *) a b. MonadUnliftIO m => m a -> m b -> m (a, b)
runConcurrentlyUsingUnliftIO2 :: ReaderT Env IO (ByteString, ByteString)
runConcurrentlyUsingUnliftIO2 = B.concurrently 
    (myReadFileWithReader "/tmp/dino1")
    (myReadFileWithReader "/tmp/dino2")

{-
• Couldn't match expected type ‘ReaderT
                                  Env IO (ByteString, ByteString)’
              with actual type ‘IO (a0, b0)’
runConcurrentlyUsingAsync2 :: ReaderT Env IO (ByteString, ByteString)
runConcurrentlyUsingAsync2 = C.concurrently 
    (myReadFileWithReader "/tmp/dino1")
    (myReadFileWithReader "/tmp/dino2")
-}

{-
GHCi> :info MonadBaseControl 
type MonadBaseControl :: (* -> *) -> (* -> *) -> Constraint
class transformers-base-0.4.5.2:Control.Monad.Base.MonadBase b m =>
      MonadBaseControl b m | m -> b where
  type StM :: (* -> *) -> * -> *
  type family StM m a
  liftBaseWith :: (RunInBase m b -> b a) -> m a
  restoreM :: StM m a -> m a
  {-# MINIMAL liftBaseWith, restoreM #-}
  	-- Defined in ‘Control.Monad.Trans.Control’
instance [safe] MonadBaseControl b m =>
                MonadBaseControl b (ReaderT r m)                  <----------------
  -- Defined in ‘Control.Monad.Trans.Control’
instance [safe] MonadBaseControl [] []
  -- Defined in ‘Control.Monad.Trans.Control’
instance [safe] MonadBaseControl Maybe Maybe
  -- Defined in ‘Control.Monad.Trans.Control’
instance [safe] MonadBaseControl IO IO                            <----------------
  -- Defined in ‘Control.Monad.Trans.Control’
instance [safe] MonadBaseControl (Either e) (Either e)
  -- Defined in ‘Control.Monad.Trans.Control’
instance [safe] MonadBaseControl ((->) r) ((->) r)
  -- Defined in ‘Control.Monad.Trans.Control’
GHCi> :info MonadUnliftIO
type MonadUnliftIO :: (* -> *) -> Constraint
class MonadIO m => MonadUnliftIO m where
  withRunInIO :: ((forall a. m a -> IO a) -> IO b) -> m b
  {-# MINIMAL withRunInIO #-}
  	-- Defined in ‘Control.Monad.IO.Unlift’
instance [safe] MonadUnliftIO m => MonadUnliftIO (ReaderT r m)    <----------------
  -- Defined in ‘Control.Monad.IO.Unlift’
instance [safe] MonadUnliftIO IO                                  <----------------
  -- Defined in ‘Control.Monad.IO.Unlift’
-}



newtype Dino = Dino Int

x :: Dino 
x = coerce (32::Int)
