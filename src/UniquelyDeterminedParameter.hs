module UniquelyDeterminedParameter where


class Foo a b | a -> b where
    bar :: a -> b


instance Foo Int Int where
    bar a = a + 1


-- Functional dependencies conflict between instance declarations:
-- instance Foo Int Int
-- 
-- instance Foo Int String where
--     bar = show