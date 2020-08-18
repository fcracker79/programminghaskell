module Chapter12 where

class MyFunctor f where
    myfmap :: (a -> b) -> f a -> f b


-- This is in the prelude, but if I do the same, as follows...
-- class MyFunctor2 (f :: * -> *) where
--    myfmap2 :: (a -> b) -> f a -> f b
-- I get:
--    Illegal kind signature: ‘* -> *’
--      Perhaps you intended to use KindSignatures
--    In the declaration for class ‘MyFunctor2’
-- BUT
--  Prelude> :info MyFunctor
--  class MyFunctor (f :: * -> *) where
--    myfmap :: (a -> b) -> f a -> f b
--    {-# MINIMAL myfmap #-}
--          -- Defined at <interactive>:3:1
