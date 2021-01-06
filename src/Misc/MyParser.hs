module Misc.MyParser where


import Data.List as L
import qualified Control.Applicative as APP
import qualified Data.Char as CHAR

type ParsingResult a = (a, String)
newtype MyParser a = MyP { parse :: String -> Maybe (ParsingResult a) }


instance Functor MyParser where
    fmap f p = MyP { parse = \s -> case parse p s of
                                       Nothing -> Nothing
                                       Just (x0, s') -> Just (f x0, s')
                    }

instance Applicative MyParser where
    pure x = MyP { parse = \s -> Just (x, s)}
    (<*>) ff pp = MyP { parse = \s -> case parse ff s of
                                          Nothing -> Nothing
                                          Just (f, s') ->  parse (fmap f pp) s'
                }


instance Monad MyParser where
    (>>=) p f = MyP { parse = \s -> case parse p s of 
                                    Nothing -> Nothing
                                    Just (x, s') -> parse (f x) s'
                    }



instance APP.Alternative MyParser where
    empty = MyP { parse = const Nothing }
    (<|>) a b = MyP { parse = \s -> case parse a s of
                                    Nothing -> parse b s
                                    x -> x
                    }


item :: MyParser Char
item = MyP (\case
                [] -> Nothing
                (x0:xs) -> Just (x0, xs)
            )
                    

expected :: String -> MyParser String
expected prefix = MyP { parse = \s -> if prefix `L.isPrefixOf` s then Just (prefix, drop (length prefix) s) else Nothing }

expectedDo :: String -> MyParser String
expectedDo [] = pure []
expectedDo (x:xs) = do
    x' <- char x
    xs' <- expectedDo xs
    return (x':xs')

sat :: (Char -> Bool) -> MyParser Char
sat f = do
    x <- item
    if f x then return x else APP.empty


digit :: MyParser Char
digit = sat CHAR.isDigit


lower :: MyParser Char
lower = sat CHAR.isLower


upper :: MyParser Char
upper = sat CHAR.isUpper


char :: Char -> MyParser Char
char c = sat (== c)

nat :: MyParser Int
nat = read <$> APP.some digit

integer :: MyParser Int
integer = nat APP.<|> do
                      char '-'
                      x <- nat
                      return $ -x

trimming :: MyParser String
trimming = APP.many (char ' ' APP.<|> char '\n')
token :: MyParser a -> MyParser a
token p = do
          trimming
          x <- p
          trimming
          return x


symbol :: String -> MyParser String
symbol s = token (expected s)


arrayOfInt :: MyParser [Int]
arrayOfInt = do
    symbol "["
    x <- integer
    xs <- APP.many $ do
                 symbol ","
                 integer
    return (x:xs)
