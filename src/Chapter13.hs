module Chapter13 where

import Control.Applicative
import Data.Char

newtype Parser p = P (String -> Maybe (p, String))

parse :: Parser p -> String -> Maybe (p, String)
parse (P p) = p

item :: Parser Char
item = P (\inp -> case inp of
                 [] -> Nothing
                 (x: xs) -> Just (x, xs))


instance Functor Parser where
    fmap f (P p) = P (\inp -> case p inp of
                              Nothing -> Nothing
                              Just (x, v) -> Just (f x, v)
                     )

instance Applicative Parser where
    pure v = P (\inp -> Just (v, inp))
    (P g) <*> fa = P (\inp -> case g inp of
                                 Nothing -> Nothing
                                 Just (gg, v) -> parse (fmap gg fa) v
                     )

instance Monad Parser where
    (P a) >>= g = P (\inp -> case a inp of
                             Nothing -> Nothing
                             Just (x, v) -> parse (g x) v
                    )

instance Alternative Parser where
    empty = P (\inp -> Nothing)
    (P a) <|> (P b) = P (\inp -> case a inp of
                                 Nothing -> b inp
                                 Just (x, v) -> Just (x, v)
                        )