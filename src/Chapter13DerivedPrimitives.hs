module Chapter13DerivedPrimitives where

import Chapter13BasicDefinition
import Chapter13MonadicDefinition
import Data.Char


conditionalParser :: (Char -> Bool) -> Parser Char
conditionalParser f = do
                                x <- item
                                if f x then return x else empty
