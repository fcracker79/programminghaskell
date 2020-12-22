module Chapter14 where
import Data.Monoid

newtype MyFunction a b = MyFunction (a -> b)

instance Semigroup b => Semigroup (MyFunction a b) where
    (<>) (MyFunction fa) (MyFunction fb) = MyFunction(\x -> fa x <> fb x)

instance Monoid b => Monoid (MyFunction a b) where
    mempty = MyFunction $ const mempty

getMyFunction (MyFunction f) = f

instance Functor (MyFunction a) where
    fmap g (MyFunction fa) = MyFunction (\x -> g (fa x))

instance Applicative (MyFunction a) where
    pure x = MyFunction (\_ -> x)
    (MyFunction g) <*> (MyFunction a) = MyFunction (\x -> ((g x) (a x)))


instance Show (MyFunction a b) where
    show (MyFunction f) = show "A function that I cannot print"
