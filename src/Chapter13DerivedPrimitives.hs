module Chapter13DerivedPrimitives where

import Chapter13BasicDefinition
import Data.Char
import Control.Applicative


conditionalParser :: (Char -> Bool) -> Parser Char
conditionalParser f = do
                                x <- item
                                if f x then return x else empty


digit :: Parser Char
digit = conditionalParser isDigit


lower :: Parser Char
lower = conditionalParser isLower

upper :: Parser Char
upper = conditionalParser isUpper

letter :: Parser Char
letter = conditionalParser isAlpha

alphanum :: Parser Char
-- alphanum = conditionalParser isAlphaNum
alphanum = letter <|> digit

char :: Char -> Parser Char
char x = conditionalParser (== x)

ident :: Parser String
ident = do
        x <- lower
        xs <- many alphanum
        return (x:xs)

nat :: Parser Int
nat = do
      x <- some digit
      return (read x)

space :: Parser ()
space = do
        many (char ' ')
        return ()
