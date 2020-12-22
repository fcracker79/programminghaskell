module BookOfMonads.Chapter11.MaybeTIdentity where

newtype Identity a = I a
newtype MyMaybeT m a = MyMaybeT {myRunMaybeT :: m (Maybe a)}

-- This works because applicative functor is (m ((Maybe a) -> (Maybe b)) ) -> m (Maybe a) -> m (Maybe b)
-- and fmap f is lifted to m through <$> operator, whereas x is of type "m (Maybe a)".
instance Monad m => Functor (MyMaybeT m) where
    fmap f (MyMaybeT x) = MyMaybeT $ do fmap f <$> x
