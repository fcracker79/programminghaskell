module BookOfMonads.Chapter9.Continuation where


newtype MyCont r a = MyCont { runCont :: (a -> r) -> r}


instance Functor (MyCont r) where
    fmap f c = MyCont (\f2 -> runCont c $ f2 . f)


instance Applicative (MyCont r) where
    pure a = MyCont (\f -> f a)
    (<*>) (MyCont ffab) (MyCont ffa) = MyCont (\fb -> ffab (\fab -> ffa $ fb . fab))


instance Monad (MyCont r) where
    (>>=) (MyCont ffa) f = MyCont (\fb -> ffa (\a -> runCont (f a) fb))



toCont a = MyCont (\f -> f a)

fromCont (MyCont c) = c id
