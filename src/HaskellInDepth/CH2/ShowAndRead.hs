
{-# LANGUAGE StandaloneDeriving #-}
module HaskellInDepth.CH2.ShowAndRead where

import Data.String (IsString(..))
import Fmt
data Person = Person String (Maybe Int)
deriving instance Show Person
deriving instance Read Person
deriving instance Eq Person

instance IsString Person where
    fromString name = Person name Nothing


data Expr a = Lit a
    | Add (Expr a) (Expr a)
    | Mult (Expr a) (Expr a)


instance (Show a) => Show (Expr a) where
    showsPrec p e = case e of
        Lit x -> showsPrec p x
        (Add e1 e2) -> myshow p 5 "+" e1 e2
        (Mult e1 e2) -> myshow p 6 "*" e1 e2
        where myshow :: Show a => Int -> Int -> String -> Expr a -> Expr a -> ShowS
              myshow pp pc op e1 e2 = showParen (pp > pc) expr
                where expr = showsPrec pc e1 . (\x -> " " ++ op ++ " " ++ x) . showsPrec pc e2

main :: IO ()
main = do
    let expr1 = Mult (Add (Lit 2) (Mult (Lit 3) (Lit 3))) (Lit 5)
    let expr2 = Add (Add (Lit 1) (Mult (Add (Lit 1) (Lit 2)) (Add (Lit 2) (Mult (Lit 2) (Add (Lit 1) (Lit 2)))))) (Add (Lit 1) (Mult (Lit 3) (Lit 2)))
    print $ "Expr1:" ++ show expr1
    print $ "Expr2:" ++ show expr2
