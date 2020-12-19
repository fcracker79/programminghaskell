module BookOfMonads.Chapter11.MaybeTIdentity where

newtype Identity a = I a
newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

-- This works because applicative functor is (m ((Maybe a) -> (Maybe b)) ) -> m (Maybe a) -> m (Maybe b)
-- and fmap f is lifted to m through <$> operator.
instance Monad m => Functor (MaybeT m) where
    fmap f (MaybeT x) = MaybeT $ do fmap f <$> x

