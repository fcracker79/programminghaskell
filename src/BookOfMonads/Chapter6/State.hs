module BookOfMonads.Chapter6.State where


newtype MyState s a = MyState { runState :: s -> (a, s) }

get :: MyState s s
get = MyState (\s -> (s, s))

put :: s -> MyState s ()
put s = MyState (\_ -> ((), s))

modify :: (s -> s) -> MyState s ()
modify f = do
    s <- get
    put $ f s


evalState :: MyState s a -> s -> a
evalState (MyState f) = fst . f
execState :: MyState s a -> s -> s
execState (MyState f) = snd . f

instance Functor (MyState s) where
    fmap f (MyState r) = MyState (\s -> let (a, s') = r s in (f a, s'))


instance Applicative (MyState s) where
    pure a = MyState (\s -> (a, s))
    (<*>) (MyState rf) (MyState ra) = MyState (\s -> let (f, s') = rf s
                                                         (a, s'') = ra s'
                                                     in (f a, s''))


instance Monad (MyState s) where
    (>>=) (MyState ra) f = MyState (\s -> let (a, s') = ra s
                                              MyState rb = f a
                                          in (rb s'))


playWithState0 :: MyState String a -> MyState String a
playWithState0 x = do
      -- a
      y <- x
      modify (flip (++) " [MODIFIED]")
      return y
