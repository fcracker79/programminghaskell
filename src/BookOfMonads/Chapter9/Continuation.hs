module BookOfMonads.Chapter9.Continuation where


data MyCont r a = MyCont { runCont :: (a -> r) -> r}


instance Functor (MyCont r) where
    fmap f c = MyCont (\f2 -> runCont c $ f2 . f)


instance Applicative (MyCont r) where
    pure a = MyCont (\f -> f a)
    (<*>) (MyCont ffab) (MyCont ffa) = MyCont (\fb -> ffab (\fab -> ffa $ fb . fab))


instance Monad (MyCont r) where
    (>>=) (MyCont ffa) f = MyCont (\fb -> (ffa (\a -> runCont (f a) fb)))


toCont :: a (forall r. MyCont r a)
toCont a = MyCont (\f -> f a)

fromCont :: (forall r. MyCont r a) -> a
fromCont (MyCont c) = c id
