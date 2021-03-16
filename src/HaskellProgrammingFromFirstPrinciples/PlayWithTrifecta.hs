module HaskellProgrammingFromFirstPrinciples.PlayWithTrifecta where


import Text.Trifecta
import Control.Monad.State (runStateT)


two :: Parser Char
two = char '1' >> char '2'


stop :: Parser a
stop = unexpected "stop"


string2 s = case s of 
    "" -> mempty
    (c:cs) -> char c >> string2 cs

ps :: Parser a -> String -> Either String a
ps p s = case parseString p mempty s of
    Failure e -> Left $ show e 
    Success a -> Right a