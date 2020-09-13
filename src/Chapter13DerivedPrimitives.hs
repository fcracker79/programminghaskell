module Chapter13DerivedPrimitives where

import Chapter13BasicDefinition
import Data.Char
import Control.Applicative


conditionalParser :: (Char -> Bool) -> Parser Char
conditionalParser f = do
                                x <- item
                                if f x then return x else empty
