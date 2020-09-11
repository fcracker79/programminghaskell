module Chapter13BasicDefinition where

newtype Parser p = P (String -> Maybe (p, String))

parse :: Parser p -> String -> Maybe (p, String)
parse (P p) = p

item :: Parser Char
item = P (\inp -> case inp of
                 [] -> Nothing
                 (x: xs) -> Just (x, xs))
