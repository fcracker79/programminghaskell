module BookOfMonads.Chapter1 where

newtype StuffWithState a = StuffWithState (Int -> (a, Int))

unwrap :: (StuffWithState a) -> (Int -> (a, Int))
unwrap (StuffWithState x) = x


instance Functor StuffWithState where
    fmap f (StuffWithState a) = StuffWithState (\i -> let (a', i') = a i in (f a', i'))

instance Applicative StuffWithState where
    pure x = StuffWithState (\i -> (x, i))
    (<*>) (StuffWithState f) (StuffWithState a) = StuffWithState (\i -> let (f', i') = f i
                                                                            (a', i'') = a i'
                                                                        in (f' a', i''))

instance Monad StuffWithState where
    --return x = StuffWithState (\i -> (x, i))
    (>>=) (StuffWithState a) f = StuffWithState (\i -> let (x, i') = a i in unwrap (f x) i')


data MyState s a = MyState s a deriving(Show)


instance (Monoid s) => Functor (MyState s) where
    fmap f (MyState s a) = MyState s (f a)

instance (Monoid s) => Applicative (MyState s) where
    pure x = MyState mempty x
    (<*>) (MyState sf f) (MyState s a) = MyState (mappend sf s) (f a)

instance (Monoid s) => Monad (MyState s) where
    (>>=) (MyState s a) f = let (MyState sb b) = f a in MyState (mappend s sb) b

instance (Monoid s, Monoid a) => Monoid (MyState s a) where
    mempty = MyState mempty mempty
    mappend (MyState sa a) (MyState sb b) = MyState (mappend sa sb) (mappend a b)

-- instance (Monoid s) => Monoid (MyState s Int) where
--     mempty = MyState mempty 0
--     mappend (MyState sa a) (MyState sb b) = MyState (mappend sa sb) (a + b)

playWithStateMonad :: Int -> Int -> MyState String String
playWithStateMonad 1 l = MyState ("l" ++ (show l)) "1"
playWithStateMonad n l = do
    let prev = playWithStateMonad (n - 1) (l + 1)
    let cur = MyState ("l" ++ (show l)) $ show n
    let result = mappend prev cur
    result


myunwrap :: Monoid s => MyState s (MyState s a) -> MyState s a
myunwrap x = do
    y <- x
    y


mytest = do
    x <- playWithStateMonad 1 1
    let y = x ++ "1"
    return "0"


myarray = [v1 | v1 <- playWithStateMonad 1 1, v2 <- playWithStateMonad 2 2, v3 <- playWithStateMonad 3 3]
dino = do
    v1 <- playWithStateMonad 1 1
    v2 <- playWithStateMonad 2 2
    return v1
