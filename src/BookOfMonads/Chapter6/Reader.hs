module BookOfMonads.Chapter6.Reader where


newtype MyReader r a = MyReader { runReader :: r -> a}


instance Functor (MyReader r) where
    fmap f (MyReader fr) = MyReader (f . fr)


instance Applicative (MyReader r) where
    pure x = MyReader (\_ -> x)
    (<*>) (MyReader fr) (MyReader ar) = MyReader (\r -> (fr r) (ar r))


instance Monad (MyReader r) where
    (>>=) (MyReader ra) f = MyReader (\r -> runReader (f (ra r)) r)


ask :: MyReader r r
ask = MyReader id

asks :: (r -> a) -> MyReader r a
asks f = f <$> ask

withReader :: (r -> s) -> MyReader s a -> MyReader r a
withReader f (MyReader r) = MyReader (r . f)

-- Boh, seems the same
-- asks2 :: (r -> a) -> MyReader r a
-- asks2 f = MyReader f

data MyConfig f1 f2 f3 = MyConfig { f1 :: String, f2 :: Int, f3 :: [Int]}


getConf :: MyReader (MyConfig f1 f2 f3) String
getConf = do
    v1 <- asks f1
    v2 <- show <$> (asks f2)
    v3 <- withReader (show . f3) ask
    return (v1 ++ v2 ++ v3)
