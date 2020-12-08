module BookOfMonads.Chapter7 where

import Control.Applicative
import Control.Monad
import Text.Read
import Control.Monad.Except


data MyEither l r = MyLeft l | MyRight r deriving(Show)


instance Functor (MyEither l) where
    fmap f (MyLeft l) = MyLeft l
    fmap f (MyRight r) = MyRight (f r)


instance Applicative (MyEither l) where
    pure x = MyRight x
    (<*>) (MyLeft l) _ = MyLeft l
    (<*>) (MyRight f) (MyLeft l) = MyLeft l
    (<*>) (MyRight f) (MyRight a) = MyRight (f a)


instance Monad (MyEither l) where
    (>>=) (MyLeft l) _ = MyLeft l
    (>>=) (MyRight r) f = f r


instance Monoid l => Alternative (MyEither l) where
    empty = MyLeft mempty
    (<|>) (MyLeft l) (MyLeft r) = MyLeft (l `mappend` r)
    (<|>) (MyLeft l) (MyRight r) = MyRight r
    (<|>) (MyRight r) _ = MyRight r


instance Monoid l => MonadPlus (MyEither l) where
    mzero = MyLeft mempty
    mplus (MyLeft l) (MyLeft r) = MyLeft (l `mappend` r)
    mplus (MyLeft l) (MyRight r) = MyRight r
    mplus (MyRight r) _ = MyRight r


newtype MySum = MySum Int deriving(Show)
instance Monoid MySum where
    mempty = MySum 0
    mappend (MySum a) (MySum b) = MySum (a + b)


data AgeError = NotANumber | NegativeNumber deriving(Show)

validateAge :: String -> Either AgeError Int
validateAge s = case (readMaybe s::(Maybe Int)) of
                Nothing -> throwError NotANumber
                Just x -> if x >= 0 then (Right x) else (throwError NegativeNumber)

safeValidateAge s = validateAge s `catchError` (\e ->
                    case e of
                        NotANumber -> safeValidateAge("0")
                        NegativeNumber -> safeValidateAge(tail s)
                    )
