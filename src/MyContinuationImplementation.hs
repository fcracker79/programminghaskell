module MyContinuationImplementation where


import Control.Exception.Base (SomeException(SomeException), AsyncException (StackOverflow))
import Control.Exception (finally, catch, throwIO)
import Control.Monad.IO.Class (MonadIO(..))
newtype MyCont r a = MyCont ((a -> r) -> r)
mycont :: MyCont r a -> (a -> r) -> r
mycont (MyCont f) = f

-- f: a -> b
-- ma = (a -> r) -> r
-- mb = (b -> r) -> r
-- fa = (a -> r)
-- fb = (b -> r)
instance Functor (MyCont r) where
    fmap f (MyCont ma) = MyCont (\fb -> ma (fb . f))

-- mf = ( ( a -> b ) -> r) -> r ) = ff -> r = (fab -> r) -> r
-- ma = ( a -> r ) -> r = fa -> r
-- mb = ( b -> r) -> r = fb -> r
-- ff = ( a -> b ) -> r)
-- fab = a -> b
-- fa = a -> r
instance Applicative (MyCont r) where
    pure a = MyCont $ \f -> f a
    (MyCont mf) <*> (MyCont ma) = MyCont (\fb -> mf (\fab -> ma (\a -> fb (fab a)) ))
--                                                   ^       ^  ^                ^
--                                                   |       |  |---- a -> r ----|
--                                                   |       |-------- r --------|
--                                                   |------ (a -> b) -> r ------|
instance Monad (MyCont r) where
    MyCont ma >>= f = MyCont $ \fb -> ma (\a -> mycont (f a) fb)


-- instance MonadIO m => MonadIO (MyCont (m r)) where
--     liftIO x = undefined