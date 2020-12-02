module BookOfMonads.Chapter6.Reader where


newtype MyReader r a = MyReader { runReader :: r -> a}


instance Functor (MyReader r) where
    fmap f (MyReader fr) = MyReader (f . fr)


instance Applicative (MyReader r) where
    pure x = MyReader (\_ -> x)
    (<*>) (MyReader fr) (MyReader ar) = MyReader (\r -> (fr r) (ar r))