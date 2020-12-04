module BookOfMonads.Chapter6.Writer where


newtype MyWriter w a = MyWriter { runWriter :: (w, a) }


instance Functor (MyWriter w) where
    fmap f (MyWriter (w, a)) = MyWriter (w, f a)


instance Monoid w => Applicative (MyWriter w) where
    pure a = MyWriter (mempty, a)
    (<*>) (MyWriter (wf, f)) (MyWriter (w, a)) = MyWriter (mappend wf w, f a)


instance Monoid w => Monad (MyWriter w) where
    (>>=) (MyWriter (w, a)) f = let MyWriter (wb, b) = f a in MyWriter (w `mappend` wb, b)
