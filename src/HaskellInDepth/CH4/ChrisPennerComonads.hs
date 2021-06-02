module HaskellInDepth.CH4.ChrisPennerComonads where


import Control.Comonad
data Stream a = a :> Stream a


instance Functor Stream where
    fmap f (a :> s) = f a :> fmap f s


{-
    
    Argument about `duplicate`
    For some reasons that are not clear at the time of writing, the Comonad instance of Stream
    is a stream where each element is the `dropS <idx>` of the original stream.
    
    So, given a stream of a :> b :> c :> ...
    we have a Stream (Streeam a) as follows:
        (a :> b :> c :> ...) :> (b :> c :> ...) :> (c :> ...) :> ...

    Argument about `extract`
    The `extract` function extracts something very special, that is, the root of the stream.
-}
instance Comonad Stream where
    duplicate (a :> s) = (a :> s) :> duplicate s
    extract (a :> _) = a


fromList :: [a] -> Stream a
fromList a = go (cycle a)
    where go (x: xs) = x :> fromList xs

countStream :: Stream Int 
countStream = fromList [0..]


ix :: Int -> Stream a -> a
ix n _ | n < 0 = error "Negative index"
ix 0 (a :> _) = a
ix n (_ :> s) = ix (pred n) s

dropSStupid :: Int -> Stream a -> Stream a
dropSStupid n s | n < 0 = s
dropSStupid n (_ :> x) = dropSStupid (pred n) x

-- `duplicate` allows to turn queries into mutations, as `duplicate` creates alla the possible views of the context and
-- the query chooses a version of the original object
dropS :: Int -> Stream a -> Stream a
dropS n s = ix n $ duplicate s

ixUsingDropS :: Int -> Stream a -> a
ixUsingDropS n s = extract $ dropS n s
