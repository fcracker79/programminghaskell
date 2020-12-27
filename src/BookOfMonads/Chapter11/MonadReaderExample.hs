module BookOfMonads.Chapter11.MonadReaderExample () where

import qualified Control.Monad.Trans.Maybe as M
import qualified Control.Monad.Trans.Reader as R


class Monad m => MonadReader r m | m -> r where
    ask :: m r
    local :: (r -> r) -> m a -> m a


instance Monad m => MonadReader r (R.ReaderT r m) where
    ask = R.ReaderT return
    local f m = R.ReaderT $ R.runReaderT m . f


instance MonadReader r m => MonadReader r (M.MaybeT m) where
    ask = M.MaybeT $ M.runMaybeT ask -- (1)
    local f m = M.MaybeT $ M.runMaybeT (local f m)


-- This function goes into a loop, because of (1) is a recursive form 
-- and NOT a delegation of the MonadReader r m (to be verified with Functional Programming guys)
testMyAsk :: M.MaybeT (R.ReaderT String IO) ()
testMyAsk = do
    v <- ask

    -- how to deal with IO?
    -- and where is the Maybe monad here?
    return ()
