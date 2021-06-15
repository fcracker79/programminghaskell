{-# LANGUAGE DeriveFunctor #-}
module HaskellInDepth.CH4.ChrisPennerComonads where


import Control.Comonad
import qualified Data.Map as M


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

    Laws
     - extract (duplicate w) = w
       This law says that the `duplicate` function must keep the original w in a special position.
     - extract <$> duplicate w = w
       This law says that the focused slot of each view must match the slot it's stored in after duplicating.
       In case of stream, we apply `extract` to each element of the duplicated stream, which provides the root
       of each element. Then we wrap it back to a stream, which must be the original stream provided that the
       root of each lifted stream matches the element at the same slot in the original stream.

       Mi sono imbattuto in una legge delle comonadi, ovvero:
       `extract <$> duplicate w = w`
       Questa legge mi è illuminante in tutte le comonadi che ripetono se stesse:
       - Stream
       - Tuple
       - Funzioni che restituiscono monoidi
       - Moore
       Il fatto che una comonade ripeta se stessa, ovvero abbia una natura "frattale", è una caratteristica intrinseca delle comonadi.
       Es. in Stream ho `a :> Stream a`

    `extends` runs a query over each slot's view.
    E infatti `extend f = fmap f . duplicate`, ovvero applico la trasformazione `f` dentro ciascuno slot prodotto dalla duplicazione.
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


-- Video 2: warehouse
data Store s a = Store (s -> a) s deriving(Functor)
inventory :: M.Map Int String 
inventory = M.fromList [
    (0, "Fidget spinners"),
    (1, "Books"),
    (2, "Guitars"),
    (3, "Laptops")
    ]




store :: (s -> a) -> s -> Store s a
store = Store
warehouse :: Store Int (Maybe String)
warehouse = store (`M.lookup` inventory) 1

pos :: Store s a -> s
pos (Store _ s) = s

peek :: s -> Store s a -> a
peek s (Store f _) = f s

peeks :: (s -> s) -> Store s a -> a
peeks g (Store f s) = f (g s)

seek :: s -> Store s a -> Store s a
seek s (Store f _) = Store f s

seeks :: (s -> s) -> Store s a -> Store s a
seeks g (Store f s) = Store f (g s)

experiment :: Functor f => (s -> f s) -> Store s a -> f a
experiment search (Store f s) = f <$> search s


instance Comonad (Store s) where
  extract (Store f s) = f s
  duplicate (Store f s) =
      Store (\s' -> Store f s') s
  extend g st = g <$> duplicate st


squared :: Store Integer Integer
squared = Store (^2) 10
