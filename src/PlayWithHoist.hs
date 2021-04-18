module PlayWithHoist where


import Control.Monad.Reader ( ReaderT (runReaderT) )
import Control.Monad.Trans.Maybe ( MaybeT )
import Control.Monad.Morph ( MFunctor(..) )
import Control.Monad.Reader.Class (ask)
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.Monad.State ( StateT (runStateT), MonadState(state) )


f :: (ReaderT String IO a -> StateT String IO a)
f r = do
    liftIO result
    where result = runReaderT r "fr"


a :: MaybeT (ReaderT String IO) ()
a = return ()



b :: MaybeT (ReaderT String IO) a -> MaybeT (StateT String IO) a
b = hoist f

b2 :: MaybeT (StateT String IO) ()
b2 = hoist f a