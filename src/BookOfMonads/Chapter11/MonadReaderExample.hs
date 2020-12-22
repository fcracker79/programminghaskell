module BookOfMonads.Chapter11.MonadReaderExample where

import Control.Monad.Trans.Maybe

class Monad m => MonadReader r m | m -> r where
    ask :: m r
    local :: (r -> r) -> m a -> m a


instance MonadReader r m => MonadReader r (MaybeT m) where
    ask = MaybeT $ runMaybeT ask
    local f m = MaybeT $ runMaybeT (local f m)
