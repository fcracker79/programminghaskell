module BookOfMonads.Chapter10.TestCompose where


newtype (f :.: g) a = Compose (f (g a))


instance (Functor f, Functor g) => Functor (f :.: g) where
    fmap f (Compose x) = Compose (fmap (fmap f) x)

-- class Functor f, Foldable f => Traversable f where
--     traverse :: Applicative m => (a -> mb) -> f a -> m (f b)

instance (Foldable (f :.: g), Traversable f, Traversable g) => Traversable (f :.: g) where
    traverse fun (Compose x) = Compose <$> traverse (traverse fun) x


composeWithInt :: (:.:) [] Maybe Integer
composeWithInt = Compose [Just 1]

composeWithString :: (:.:) [] Maybe String
composeWithString = fmap show composeWithInt