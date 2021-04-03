{-# LANGUAGE GADTs #-}

module PlayWithPhantomTypes where


import Data.Dynamic ( toDyn, fromDynamic )
import Text.Format

data Term t where
    Zero :: Term Int
    Succ :: Term Int -> Term Int
    Pred :: Term Int -> Term Int
    IsZero :: Term Int -> Term Bool 
    If :: Term Bool -> Term x -> Term x -> Term x


instance Show t => Show (Term t) where
    show Zero = "Term Zero"
    show (Succ x) = format "Succ ({0})" [show x]
    show (Pred x) = format "Pred ({0})" [show x]
    show (IsZero x) = format "Is {0} Zero ?" [show x]
    show (If x a b) = format "If {0} then {1} else {2}" [show x, show a, show b]

eval :: Term t -> t
eval Zero = 0
eval (Succ x) = eval x + 1
eval (Pred x) = eval x - 1
eval (IsZero x) = eval x == 0
eval (If c a b) = if eval c then eval a else eval b


x :: [Maybe (Term Int)]
x = fmap fromDynamic [toDyn Zero, toDyn (Succ Zero)] :: [Maybe (Term Int)]

main :: IO ()
main = do
    print x
