module BookOfMonads.Chapter10.TestCompose where


newtype (f :.: g) a = Compose (f (g a))


instance (Functor f, Functor g) => Functor (f :.: g) where
    fmap f (Compose x) = Compose (fmap (fmap f) x)


composeWithInt :: (:.:) [] Maybe Integer
composeWithInt = Compose [Just 1]

composeWithString :: (:.:) [] Maybe String
composeWithString = fmap show composeWithInt